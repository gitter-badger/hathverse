{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Hathverse.View.Signup where

import Data.Int (Int64)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Hathverse.View.Common
import Hathverse.Db ()
import Lucid


signupResult :: String -> Html()
signupResult userid = toHtml userid

loginView ::  Html ()
signupView = withTitleBody "login" $ do
        script_ "function check(){if(userform.password.value!=userform.password2.value){            alert('The password must be equal');            return false;        }        else{           return true;        }    }"
        form_ [action_ "/login", method_ "post" , name_ "loginform"] $ do
        table_ [class_ "table table-bordered table-hover"] $ do
            tr_ $ do
                td_ "username [0-9a-zA-Z]+"
                td_ $ input_ [type_ "text", name_ "username",pattern_ "[0-9a-zA-Z]+",maxlength_ "20"]
            tr_ $ do
                td_ "password"
                td_ $ input_ [type_ "password", name_ "password",maxlength_ "20"]
            tr_ $ do
                td_ ""
                td_ $ input_ [type_ "submit", value_ "login"]
