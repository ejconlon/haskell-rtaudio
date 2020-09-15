module Sound.RtAudio
  ( Api (..)
  , Audio
  , Error (..)
  , DeviceInfo (..)
  , StreamParams (..)
  , StreamOptions (..)
  , Format
  , formatInt8
  , formatInt16
  , formatInt24
  , formatInt32
  , formatFloat32
  , formatFloat64
  , StreamStatus
  , statusInputUnderflow
  , statusOutputUnderflow
  , StreamFlags
  , flagsNoninterleaved
  , flagsMinimizeLatency
  , flagsScheduleRealtime
  , flagsAlsaUseDefault
  , flagsJackDontConnect
  , getVersion
  , getCompiledApis
  , apiName
  , apiDisplayName
  , apiByName
  , createAudio
  , audioCurrentApi
  , audioDeviceCount
  , audioGetDeviceInfo
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Data.Bits ((.&.))
import Foreign (Ptr, alloca, nullPtr, peek, peekArray)
import Foreign.C (CInt (..), peekCString, withCString)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Sound.RtAudio.Foreign

-- | An internal RtAudio error
newtype Error = Error String deriving (Eq, Show)
instance Exception Error

newtype Audio = Audio { unAudio :: ForeignPtr AudioInternal } deriving (Eq, Show)

-- Throws an error if present.
guardError :: Ptr AudioInternal -> IO ()
guardError ast = do
  ce <- rtaudio_error ast
  unless (ce == nullPtr) $ do
    e <- peekCString ce
    throwIO (Error e)

withAudioStructUnguarded :: Audio -> (Ptr AudioInternal -> IO a) -> IO a
withAudioStructUnguarded (Audio fptr) = withForeignPtr fptr

withAudioStruct :: Audio -> (Ptr AudioInternal -> IO a) -> IO a
withAudioStruct audio f = withAudioStructUnguarded audio (\ptr -> f ptr <* guardError ptr)

-- | Returns the version string of the RtAudio API (e.g. "5.1.0").
getVersion :: IO String
getVersion = rtaudio_version >>= peekCString

-- | Returns a list of all supported RtMidi APIs.
getCompiledApis :: IO [Api]
getCompiledApis = do
  n <- rtaudio_get_num_compiled_apis
  ptr <- rtaudio_compiled_api
  arr <- peekArray (fromIntegral n) ptr
  pure (fmap toApi arr)

-- | Returns the internal name of the RtMidi API.
apiName :: Api -> IO String
apiName api = rtaudio_api_name (fromApi api) >>= peekCString

-- | Returns the display name of the RtMidi API.
apiDisplayName :: Api -> IO String
apiDisplayName api = rtaudio_api_display_name (fromApi api) >>= peekCString

-- | Looks up the RtMidi API by its /internal/ name.
--
-- Note that this is the name from 'apiName', not 'apiDisplayName'.
-- If not found, returns 'UnspecifiedApi'.
apiByName :: String -> IO Api
apiByName name = fmap toApi (withCString name rtaudio_compiled_api_by_name)

-- | Creates an 'Audio' interface using the given API, or throws an error.
createAudio :: Api -> IO Audio
createAudio api = do
  ast <- rtaudio_create (fromApi api)
  fptr <- newForeignPtr rtaudio_destroy ast
  guardError ast
  pure (Audio fptr)

-- | Returns the API used by the 'Audio' interface.
audioCurrentApi :: Audio -> IO Api
audioCurrentApi = flip withAudioStructUnguarded (fmap toApi . rtaudio_current_api)

-- | Returns the number of devices available to the 'Audio' interface.
audioDeviceCount :: Audio -> IO Int
audioDeviceCount = flip withAudioStructUnguarded (fmap fromIntegral . rtaudio_device_count)

-- | Returns 'DeviceInfo' for the given audio device.
-- The index must be less than or equal to the count returned by 'audioDeviceCount',
-- otherwise it throws an 'Error'.
audioGetDeviceInfo :: Audio -> Int -> IO DeviceInfo
audioGetDeviceInfo audio index = withAudioStruct audio $ \ptr -> alloca $ \dptr -> do
  rtaudio_get_device_info ptr (fromIntegral index) dptr
  peek dptr
