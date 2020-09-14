{-# LANGUAGE DerivingVia #-}

module Sound.RtAudio where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Data.Bits ((.&.))
import Foreign (Ptr, nullPtr, peekArray)
import Foreign.C (CInt (..), peekCString, withCString)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Sound.RtAudio.Flag (Flag, BitFlag (..))
import Sound.RtAudio.Foreign

newtype StreamStatus = StreamStatus { unStreamStatus :: Int }
  deriving (Eq, Show)
  deriving (Semigroup, Monoid, Flag) via (BitFlag Int)

statusInputUnderflow, statusOutputUnderflow :: StreamStatus
statusInputUnderflow = StreamStatus 0x1
statusOutputUnderflow = StreamStatus 0x2

newtype StreamFlags = StreamFlags { unStreamFlags :: Int }
  deriving (Eq, Show)
  deriving (Semigroup, Monoid, Flag) via (BitFlag Int)

flagsNoninterleaved, flagsMinimizeLatency, flagsScheduleRealtime, flagsAlsaUseDefault, flagsJackDontConnect :: StreamFlags
flagsNoninterleaved = StreamFlags 0x1
flagsMinimizeLatency = StreamFlags 0x2
flagsHogDevice = StreamFlags 0x4
flagsScheduleRealtime = StreamFlags 0x8
flagsAlsaUseDefault = StreamFlags 0x10
flagsJackDontConnect = StreamFlags 0x20

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
  toEnum i = error ("Undefined Api: " ++ show i)

-- typedef enum rtaudio_error {
--   RTAUDIO_ERROR_WARNING,           /*!< A non-critical error. */
--   RTAUDIO_ERROR_DEBUG_WARNING,     /*!< A non-critical error which might be useful for debugging. */
--   RTAUDIO_ERROR_UNSPECIFIED,       /*!< The default, unspecified error type. */
--   RTAUDIO_ERROR_NO_DEVICES_FOUND,  /*!< No devices found on system. */
--   RTAUDIO_ERROR_INVALID_DEVICE,    /*!< An invalid device ID was specified. */
--   RTAUDIO_ERROR_MEMORY_ERROR,      /*!< An error occured during memory allocation. */
--   RTAUDIO_ERROR_INVALID_PARAMETER, /*!< An invalid parameter was specified to a function. */
--   RTAUDIO_ERROR_INVALID_USE,       /*!< The function was called incorrectly. */
--   RTAUDIO_ERROR_DRIVER_ERROR,      /*!< A system driver error occured. */
--   RTAUDIO_ERROR_SYSTEM_ERROR,      /*!< A system error occured. */
--   RTAUDIO_ERROR_THREAD_ERROR,      /*!< A thread error occured. */
-- } rtaudio_error_t;

data ErrorCode =
    WarningCode
  | DebugWarningCode
  | UnspecifiedCode
  | NoDevicesFoundCode
  | InvalidDeviceCode
  | MemoryErrorCode
  | InvalidParameterCode
  | InvalidUseCode
  | DriverErrorCode
  | SystemErrorCode
  | ThreadErrorCode
  deriving (Eq, Show)

instance Bounded ErrorCode where
  minBound = WarningCode
  maxBound = ThreadErrorCode

instance Enum ErrorCode where
  fromEnum WarningCode = 0
  fromEnum DebugWarningCode = 1
  fromEnum UnspecifiedCode = 2
  fromEnum NoDevicesFoundCode = 3
  fromEnum InvalidDeviceCode = 4
  fromEnum MemoryErrorCode = 5
  fromEnum InvalidParameterCode = 6
  fromEnum InvalidUseCode = 7
  fromEnum DriverErrorCode = 8
  fromEnum SystemErrorCode = 9
  fromEnum ThreadErrorCode = 10
  toEnum 0 = WarningCode
  toEnum 1 = DebugWarningCode
  toEnum 2 = UnspecifiedCode
  toEnum 3 = NoDevicesFoundCode
  toEnum 4 = InvalidDeviceCode
  toEnum 5 = MemoryErrorCode
  toEnum 6 = InvalidParameterCode
  toEnum 7 = InvalidUseCode
  toEnum 8 = DriverErrorCode
  toEnum 9 = SystemErrorCode
  toEnum 10 = ThreadErrorCode
  toEnum i = error ("Undefined ErrorCode: " ++ show i)

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
