{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Hathverse.View.Signup where

import Data.Int (Int64)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Hathverse.View.Common
import Hathverse.Db (Problem(..))
import Lucid


signupResult :: String -> Html()
signupResult i = toHtml i

signupView ::  Html ()
signupView = withTitleBody "signup" $ do
        script_ "function check(){alert('');return false;}" --if(document.form.password.value==document.form.password2.value){            altert('The password arn\'t eqoul');            false;        }        else{            true;        }    }        }"
        form_ [action_ "/signup", method_ "post",onsubmit_ "return check();" ] $ do
        table_ [class_ "table table-bordered table-hover"] $ do
            tr_ $ do
                td_ "fullname"
                td_ $ input_ [type_ "text", name_ "showname"]
            tr_ $ do
                td_ "username"
                td_ $ input_ [type_ "text", name_ "username",pattern_ "[0-9a-zA-Z]*"]
            tr_ $ do
                td_ "password"
                td_ $ input_ [type_ "password", name_ "password"]
            tr_ $ do
                td_ "password"
                td_ $ input_ [type_ "password", name_ "password2"]
            tr_ $ do input_ [type_ "submit", value_ "Submit"]
