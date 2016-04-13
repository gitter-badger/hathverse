{-# LANGUAGE OverloadedStrings #-}
module Hathverse.View.Login where

import Hathverse.View.Common
import Lucid

loginView :: String ->  HtmlGen
loginView inString = withTitleBody "login" $ do
        form_ [action_ "/login", method_ "post" , name_ "loginform"] $ do
        table_ [class_ "table table-bordered table-hover"] $ do
            tr_ $ do
                td_ ""
                td_ $ toHtml inString
            tr_ $ do
                td_ "username"
                td_ $ input_ [type_ "text", name_ "username",pattern_ "[0-9a-zA-Z]+",maxlength_ "20"]
            tr_ $ do
                td_ "password"
                td_ $ input_ [type_ "password", name_ "password",maxlength_ "20"]
            tr_ $ do
                td_ ""
                td_ $ input_ [type_ "submit", value_ "login"]
