{-# LANGUAGE DeriveGeneric #-}
module Hathverse.Controller where

import GHC.Generics (Generic)
import Data.Int (Int64)
import Data.ByteString.Lazy (ByteString)
import Data.Aeson
import Control.Monad.Reader
import Lucid
import Hathverse.Db
import Hathverse.View
import Hathverse.View.Common
import Hathverse.Checker

runHtml :: HtmlGen -> Query ByteString
runHtml action = do
  maybeUser <- asks currUser
  return . flip runReader maybeUser $ renderBST action

homepage :: Query ByteString
homepage = do
  pidTitles <- allProblemIdTitles
  runHtml $ homepageView pidTitles

problemPage :: Int64 -> Query ByteString
problemPage pid = do
  prob <- getProblemById pid
  runHtml $ problemView pid prob

loginPage :: Query ByteString
loginPage = runHtml loginView

data CheckRequest = CheckRequest {
    probId :: Int64
  , solCode :: String
  } deriving Generic

instance FromJSON CheckRequest

data CheckResult = CheckResult {
    result :: String
  } deriving Generic

instance ToJSON CheckResult

checkApi :: CheckRequest -> Query CheckResult
checkApi (CheckRequest pid code) = do
  prob <- getProblemById pid
  case prob of
    Nothing -> return $ CheckResult "Problem not found."
    Just problem -> lift $ CheckResult <$> check problem code

