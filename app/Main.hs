{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Main (main) where

import Control.Monad.Reader
import System.Environment (lookupEnv)
import Web.Spock.Safe
-- import Web.Spock.Lucid
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Hathverse.Db
import Hathverse.Controller
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Crypto.PasswordStore


main :: IO ()
main = runConnPool $ \pool -> do

  let sessionCfg = Nothing  -- { sc_cookieName = "hathverse" }
      appCfg = defaultSpockCfg sessionCfg (PCPool pool) ()

  port <- maybe 3000 read <$> lookupEnv "PORT"
  runSpock port $ spock appCfg app


app :: SpockM _ (Maybe User) state ()
app = do

    middleware logStdoutDev
    middleware $ staticPolicy $ addBase "static"

    get root $ lazyBytes =<< runQuery' homepage

    get ("problems" <//> var) $ \pid ->
      lazyBytes =<< runQuery' (problemPage pid)

    get "/login" $
      lazyBytes =<< runQuery' (loginPage "Please login")

    post "/login" $ do
        u <- param' "username"
        p <- param' "password"
        users <-  runQuery' $ getUserByUsername u
        case users of
            Nothing ->
              lazyBytes =<< runQuery' (loginPage "The user don't exist")
            Just user ->
              if verifyPassword (encodeUtf8 p) $ encodeUtf8 (userPassword user)
                then do
                  writeSession  $ Just user
                  redirect "/"
                else
                  lazyBytes =<< runQuery' (loginPage "The password is wrong ")

    get "/signup" $ lazyBytes =<< runQuery' signupPage

    post "/signup" $ do
        u <- param' "username"
        f <- param' "fullname"
        p <- param' "password"
        salt <- liftIO genSaltIO
        let password =  decodeUtf8 $ makePasswordSalt (encodeUtf8 p) salt 17
        userid <- runQuery' $ addUser u f password
        lazyBytes =<< runQuery' (signupResultPage $ show userid)

    -- post "/check" $
    --    json =<< runQuery' . checkApi =<< jsonBody'

runQuery' action = do
  sess <- readSession
  runQuery $ \conn ->
    liftIO (runReaderT action (Env conn sess))
