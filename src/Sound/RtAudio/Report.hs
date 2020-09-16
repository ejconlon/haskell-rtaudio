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
  , apiRepName :: !String
  , apiRepDisplayName :: !String
  , apiRepDevCount :: !Int
  , apiRepDefaultOutput :: !Int
  , apiRepDefaultInput :: !Int
  , apiRepDeviceInfos :: ![DeviceInfo]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

data Report = Report
  { repVersion :: !String
  , repApiReports :: ![ApiReport]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

buildApiReport :: MonadIO m => Api -> m ApiReport
buildApiReport api = do
  name <- apiName api
  displayName <- apiDisplayName api
  audio <- createAudio api
  devCount <- deviceCount audio
  defOut <- getDefaultOutputDevice audio
  defIn <- getDefaultInputDevice audio
  devInfos <- traverse (getDeviceInfo audio) [0 .. devCount - 1]
  pure (ApiReport api name displayName devCount defOut defIn devInfos)

buildReport :: MonadIO m => m Report
buildReport = do
  version <- getVersion
  apis <- getCompiledApis
  apiReports <- traverse buildApiReport apis
  pure (Report version apiReports)
