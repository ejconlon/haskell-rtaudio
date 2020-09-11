module Sound.RtAudio where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Foreign (Ptr, nullPtr, peekArray)
import Foreign.C (CInt (..), peekCString, withCString)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Sound.RtAudio.Foreign

data Api
  = UnspecifiedApi
  | AlsaApi
  | PulseApi
  | OssApi
  | JackApi
  | CoreApi
  | WasApi
  | AsioApi
  | DsApi
  | DummyApi
  deriving (Eq, Show)

instance Bounded Api where
  minBound = UnspecifiedApi
  maxBound = DummyApi

instance Enum Api where
  fromEnum UnspecifiedApi = 0
  fromEnum AlsaApi = 1
  fromEnum PulseApi = 2
  fromEnum OssApi = 3
  fromEnum JackApi = 4
  fromEnum CoreApi = 5
  fromEnum WasApi = 6
  fromEnum AsioApi = 7
  fromEnum DsApi = 8
  fromEnum DummyApi = 9
  toEnum 0 = UnspecifiedApi
  toEnum 1 = AlsaApi
  toEnum 2 = PulseApi
  toEnum 3 = OssApi
  toEnum 4 = JackApi
  toEnum 5 = CoreApi
  toEnum 6 = WasApi
  toEnum 7 = AsioApi
  toEnum 8 = DsApi
  toEnum 9 = DummyApi

-- | An internal RtAudio error
newtype Error = Error String deriving (Eq, Show)
instance Exception Error

newtype Audio = Audio { unAudio :: ForeignPtr AudioStruct } deriving (Eq, Show)

withAudioStruct :: (Ptr AudioStruct -> IO a) -> Audio -> IO a
withAudioStruct f (Audio fptr) = withForeignPtr fptr f

-- | Returns the version string of the RtAudio API (e.g. "5.1.0").
getVersion :: IO String
getVersion = rtaudio_version >>= peekCString

-- TODO(ejconlon) This might be platform specific...
enumByteSize :: Int
enumByteSize = 4

-- | Returns a list of all supported RtMidi APIs.
getCompiledApis :: IO [Api]
getCompiledApis = do
  n <- rtaudio_get_num_compiled_apis
  ptr <- rtaudio_compiled_api
  arr <- peekArray (fromIntegral n) ptr
  pure (fmap (toEnum . fromIntegral) arr)

-- | Returns the internal name of the RtMidi API.
apiName :: Api -> IO String
apiName api = rtaudio_api_name (fromIntegral (fromEnum api)) >>= peekCString

-- | Returns the display name of the RtMidi API.
apiDisplayName :: Api -> IO String
apiDisplayName api = rtaudio_api_display_name (fromIntegral (fromEnum api)) >>= peekCString

-- | Looks up the RtMidi API by its /internal/ name.
--
-- Note that this is the name from 'apiName', not 'apiDisplayName'.
-- If not found, returns 'UnspecifiedApi'.
apiByName :: String -> IO Api
apiByName name = fmap (toEnum . fromIntegral) (withCString name rtaudio_compiled_api_by_name)

-- Throws an error if present.
guardError :: Ptr AudioStruct -> IO ()
guardError ast = do
  ce <- rtaudio_error ast
  unless (ce == nullPtr) $ do
    e <- peekCString ce
    throwIO (Error e)

-- | Creates an 'Audio' interface using the given API, or throws an error.
createAudio :: Api -> IO Audio
createAudio api = do
  ast <- rtaudio_create (fromIntegral (fromEnum api))
  fptr <- newForeignPtr rtaudio_destroy ast
  guardError ast
  pure (Audio fptr)

-- | Returns the API used by the 'Audio' interface.
audioCurrentApi :: Audio -> IO Api
audioCurrentApi = withAudioStruct (fmap (toEnum . fromIntegral) . rtaudio_current_api)

-- | Returns the number of devices available to the 'Audio' interface.
audioDeviceCount :: Audio -> IO Int
audioDeviceCount = withAudioStruct (fmap fromIntegral . rtaudio_device_count)
