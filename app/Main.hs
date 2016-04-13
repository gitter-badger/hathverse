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
import Data.Map

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
      lazyBytes =<< runQuery' loginPage

    post "/login" $ do
      username <- param' "username"
      password <- param' "password"
      _type <- param' "type"
      maybeUser <- runQuery' $ getUserByUsername username
      (ok, err) <-
        case _type :: String of
          "login" ->
            case maybeUser of
              Nothing -> return (False, "User not found.")
              Just user ->
                if verifyPassword (encodeUtf8 password) $ encodeUtf8 (userPassword user)
                  then do
                    writeSession $ Just user
                    return (True, "success")
                  else return (False, "Wrong password.")
          "signup" ->
            case maybeUser of
              Just _ -> return (False, "Username is already used.")
              Nothing -> do
                salt <- liftIO genSaltIO
                let hashPassword =  decodeUtf8 $ makePasswordSalt (encodeUtf8 password) salt 17
                userid <- runQuery' $ addUser username hashPassword
                writeSession =<< runQuery' (getUserByUsername username)
                return (True, show userid)
          _ -> return (False, "?")

      json (fromList [("ok" :: String, show ok), ("err", err)])

    get "/logout" $ do
      writeSession Nothing
      redirect "/login"

    post "/check" $
      json =<< runQuery' . checkApi =<< jsonBody'

runQuery' action = do
  sess <- readSession
  runQuery $ \conn ->
    liftIO (runReaderT action (Env conn sess))
