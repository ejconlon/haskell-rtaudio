{-# LANGUAGE DeriveAnyClass #-}

module Sound.RtAudio.Report
  ( ApiReport (..)
  , Report (..)
  , buildReport
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (MonadIO)
import GHC.Generics (Generic)
import Sound.RtAudio

data ApiReport = ApiReport
  { apiRepApi :: !Api
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

data Report = Report
  { repVersion :: !String
  , repApiReports :: ![ApiReport]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

buildReport :: MonadIO m => m Report
buildReport = do
  version <- getVersion
  -- TODO(ejconlon) Fill in
  let apiReports = []
  pure (Report version apiReports)
