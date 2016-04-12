{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Main (main) where

import Control.Monad.Reader
import System.Environment (lookupEnv)
import Web.Spock.Safe
import Web.Spock.Lucid
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import qualified Hathverse.Db as Db
import Hathverse.Controller
import Data.Text.Encoding (decodeUtf8,encodeUtf8)
import Crypto.PasswordStore



main :: IO ()
main = Db.runConnPool $ \pool -> do

  let sessionCfg = (defaultSessionCfg ()) { sc_cookieName = "hathverse" }
      appCfg = defaultSpockCfg sessionCfg (PCPool pool) ()

  port <- maybe 3000 read <$> lookupEnv "PORT"
  runSpock port $ spock appCfg app


app :: SpockM _ ses state ()
app = do

    middleware logStdoutDev
    middleware $ staticPolicy $ addBase "static"

    get root $
      lucid =<< runQuery' homepage

    get ("problems" <//> var) $ \pid ->
      lucid =<< runQuery' (problemPage pid)

    get "/login" $  do
        html . toStrict  $ loginPage  "Please login"

    post "/login" $ do
        u <- param' "username"
        p <- param' "password"
        dbpassowrd <-  runQuery' $ Db.getPasswordByUsername u
        case dbpassowrd of
            Nothing -> html .  toStrict $ loginPage "The user don't exist"
            Just password -> case (verifyPassword (encodeUtf8 p) $ encodeUtf8 password) of
                True -> html . toStrict $ loginPage " Pass login "
                False -> html . toStrict $  loginPage "The password is wrong "

    get "/signup" $ html . toStrict $ signupPage

    post "/signup" $ do
        u <- param' "username"
        f <- param' "fullname"
        p <- param' "password"
        salt <- liftIO genSaltIO
        let password =  decodeUtf8 $ makePasswordSalt (encodeUtf8 p) salt 17
        userid <- runQuery' $ Db.addUser u f password
        html . toStrict $ signupResultPage $ show userid

    post "/check" $
       json =<< runQuery' . checkApi =<< jsonBody'


  where runQuery' action = runQuery $ \conn -> liftIO (runReaderT action conn)

--pleaseLogin::Data.Text.Internal.Lazy.Text
-- pleaseLogin =  html . toStrict  $ loginPage  "Please login"
