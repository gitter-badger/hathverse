{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Hathverse.View.Signup where

import Data.Int (Int64)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Hathverse.View.Common
import Hathverse.Db (Problem(..),addUser,runQuery)
import Lucid


signupResult :: String -> Html()
signupResult userid = toHtml userid

signupView ::  Html ()
signupView = withTitleBody "signup" $ do
        script_ "function check(){if(userform.password.value!=userform.password2.value){            alert('The password must be equal');            return false;        }        else{           return true;        }    }"
        form_ [action_ "/signup", method_ "post",onsubmit_ "return check();" , name_ "userform"] $ do
        table_ [class_ "table table-bordered table-hover"] $ do
            tr_ $ do
                td_ "fullname"
                td_ $ input_ [type_ "text", name_ "fullname"]
            tr_ $ do
                td_ "username"
                td_ $ input_ [type_ "text", name_ "username",pattern_ "[0-9a-zA-Z]+"]
            tr_ $ do
                td_ "password"
                td_ $ input_ [type_ "password", name_ "password"]
            tr_ $ do
                td_ "password"
                td_ $ input_ [type_ "password", name_ "password2"]
            tr_ $ do
                td_ ""
                td_ $ input_ [type_ "submit", value_ "Submit"]