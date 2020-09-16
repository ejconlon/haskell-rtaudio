{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Sound.RtAudio.Foreign where

#include "rtaudio_c.h"
#include "wrapper.h"

import Control.DeepSeq (NFData)
import Data.Int (Int32)
import Data.Word (Word32, Word64)
import Foreign (FunPtr, Ptr, Storable (..), nullPtr, peekArray, plusPtr, pokeArray, pokeArray0)
import Foreign.C (CBool (..), CDouble (..), CInt (..), CString (..), CUInt (..), castCharToCChar, peekCString)
import GHC.Generics (Generic)
import Sound.RtAudio.Flag (Flag, BitFlag (..))

unCBool :: CBool -> Bool
unCBool (CBool x) = x > 0

asCBool :: Bool -> CBool
asCBool b = CBool (if b then 1 else 0)

unCInt :: CInt -> Int32
unCInt (CInt x) = x

unCUInt :: CUInt -> Word32
unCUInt (CUInt x) = x

pokeCStringLen :: Int -> CString -> String -> IO ()
pokeCStringLen mlen dst = pokeArray0 (castCharToCChar '\0') dst . fmap castCharToCChar . take (pred mlen)

newtype Format = Format { unFormat :: Word64 }
  deriving stock (Eq, Show)
  deriving newtype (NFData)
  deriving (Semigroup, Monoid, Flag) via (BitFlag Word64)

formatInt8, formatInt16, formatInt24, formatInt32, formatFloat32, formatFloat64 :: Format
formatInt8 = Format #{const RTAUDIO_FORMAT_SINT8}
formatInt16 = Format #{const RTAUDIO_FORMAT_SINT16}
formatInt24 = Format #{const RTAUDIO_FORMAT_SINT24}
formatInt32 = Format #{const RTAUDIO_FORMAT_SINT32}
formatFloat32 = Format #{const RTAUDIO_FORMAT_FLOAT32}
formatFloat64 = Format #{const RTAUDIO_FORMAT_FLOAT64}

newtype StreamFlags = StreamFlags { unStreamFlags :: Word32 }
  deriving stock (Eq, Show)
  deriving newtype (NFData)
  deriving (Semigroup, Monoid, Flag) via (BitFlag Word32)

flagsNoninterleaved, flagsMinimizeLatency, flagsScheduleRealtime, flagsAlsaUseDefault, flagsJackDontConnect :: StreamFlags
flagsNoninterleaved = StreamFlags #{const RTAUDIO_FLAGS_NONINTERLEAVED}
flagsMinimizeLatency = StreamFlags #{const RTAUDIO_FLAGS_MINIMIZE_LATENCY}
flagsHogDevice = StreamFlags #{const RTAUDIO_FLAGS_HOG_DEVICE}
flagsScheduleRealtime = StreamFlags #{const RTAUDIO_FLAGS_SCHEDULE_REALTIME}
flagsAlsaUseDefault = StreamFlags #{const RTAUDIO_FLAGS_ALSA_USE_DEFAULT}
flagsJackDontConnect = StreamFlags 0x20 -- #{const RTAUDIO_FLAGS_JACK_DONT_CONNECT}
-- TODO(ejconlon) Why does this one fail? ^^

newtype StreamStatus = StreamStatus { unStreamStatus :: Word32 }
  deriving stock (Eq, Show)
  deriving newtype (NFData)
  deriving (Semigroup, Monoid, Flag) via (BitFlag Word32)

statusInputUnderflow, statusOutputUnderflow :: StreamStatus
statusInputUnderflow = StreamStatus #{const RTAUDIO_STATUS_INPUT_OVERFLOW}
statusOutputUnderflow = StreamStatus #{const RTAUDIO_STATUS_OUTPUT_UNDERFLOW}

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
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData)

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
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData)

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

-- Will be mapped to Api
newtype ApiInternal = ApiInternal { unApiInternal :: CInt } deriving newtype (Storable)

toApi :: ApiInternal -> Api
toApi = toEnum . fromIntegral . unApiInternal

fromApi :: Api -> ApiInternal
fromApi = ApiInternal . fromIntegral . fromEnum

-- Will be mapped to ErrorCode
newtype ErrorCodeInternal = ErrorCodeInternal { unErrorCodeInternal :: CInt } deriving newtype (Storable)

toErrorCode :: ErrorCodeInternal -> ErrorCode
toErrorCode = toEnum . fromIntegral . unErrorCodeInternal

fromErrorCode :: ErrorCode -> ErrorCodeInternal
fromErrorCode = ErrorCodeInternal . fromIntegral . fromEnum

-- Opaque `rtaudio` struct
data AudioInternal

data DeviceInfo = DeviceInfo
  { diProbed :: !Int32
  , diOutputChannels :: !Word32
  , diInputChannels :: !Word32
  , diDuplexChannels :: !Word32
  , diIsDefaultOutput :: !Bool
  , diIsDefaultInput :: !Bool
  , diNativeFormats :: !Word32
  , diPreferredSampleRate :: !Word32
  , diSampleRates :: ![Int32]
  , diName :: !String
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

numSampleRates :: Int
numSampleRates = #{const NUM_SAMPLE_RATES}

maxNameLength :: Int
maxNameLength = #{const MAX_NAME_LENGTH}

instance Storable DeviceInfo where
  sizeOf _ = #{size rtaudio_device_info_t}
  alignment _ = #{alignment rtaudio_device_info_t}
  poke ptr s = do
    #{poke rtaudio_device_info_t, probed} ptr (CInt (diProbed s))
    #{poke rtaudio_device_info_t, output_channels} ptr (CUInt (diOutputChannels s))
    #{poke rtaudio_device_info_t, input_channels} ptr (CUInt (diInputChannels s))
    #{poke rtaudio_device_info_t, duplex_channels} ptr (CUInt (diDuplexChannels s))
    #{poke rtaudio_device_info_t, is_default_output} ptr (asCBool (diIsDefaultOutput s))
    #{poke rtaudio_device_info_t, is_default_input} ptr (asCBool (diIsDefaultInput s))
    #{poke rtaudio_device_info_t, native_formats} ptr (CUInt (diNativeFormats s))
    #{poke rtaudio_device_info_t, preferred_sample_rate} ptr (CUInt (diPreferredSampleRate s))
    pokeArray (#{ptr rtaudio_device_info_t, sample_rates} ptr) (take numSampleRates (fmap CInt (diSampleRates s)))
    pokeCStringLen maxNameLength (#{ptr rtaudio_device_info_t, name} ptr) (diName s)
  peek ptr =
    DeviceInfo <$>
      fmap unCInt (#{peek rtaudio_device_info_t, probed} ptr) <*>
      fmap unCUInt (#{peek rtaudio_device_info_t, output_channels} ptr) <*>
      fmap unCUInt (#{peek rtaudio_device_info_t, input_channels} ptr) <*>
      fmap unCUInt (#{peek rtaudio_device_info_t, duplex_channels} ptr) <*>
      fmap unCBool (#{peek rtaudio_device_info_t, is_default_output} ptr) <*>
      fmap unCBool (#{peek rtaudio_device_info_t, is_default_input} ptr) <*>
      fmap unCUInt (#{peek rtaudio_device_info_t, native_formats} ptr) <*>
      fmap unCUInt (#{peek rtaudio_device_info_t, preferred_sample_rate} ptr) <*>
      fmap (fmap unCInt) (peekArray numSampleRates (#{ptr rtaudio_device_info_t, sample_rates} ptr)) <*>
      peekCString (#{ptr rtaudio_device_info_t, name} ptr)

data StreamParams = StreamParams
  { spDeviceId :: !Word32
  , spNumChannels :: !Word32
  , spFirstChannel :: !Word32
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

instance Storable StreamParams where
  sizeOf _ = #{size rtaudio_stream_parameters_t}
  alignment _ = #{alignment rtaudio_stream_parameters_t}
  poke ptr s = do
    #{poke rtaudio_stream_parameters_t, device_id} ptr (CUInt (spDeviceId s))
    #{poke rtaudio_stream_parameters_t, num_channels} ptr (CUInt (spNumChannels s))
    #{poke rtaudio_stream_parameters_t, first_channel} ptr (CUInt (spFirstChannel s))
  peek ptr =
    StreamParams <$>
      fmap unCUInt (#{peek rtaudio_stream_parameters_t, device_id} ptr) <*>
      fmap unCUInt (#{peek rtaudio_stream_parameters_t, num_channels} ptr) <*>
      fmap unCUInt (#{peek rtaudio_stream_parameters_t, first_channel} ptr)

data StreamOptions = StreamOptions
  { soFlags :: !StreamFlags
  , soNumBuffers :: !Word32
  , soPriority :: !Int32
  , soName :: !String
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

instance Storable StreamOptions where
  sizeOf _ = #{size rtaudio_stream_options_t}
  alignment _ = #{alignment rtaudio_stream_options_t}
  poke ptr s = do
    #{poke rtaudio_stream_options_t, flags} ptr (CUInt (unStreamFlags (soFlags s)))
    #{poke rtaudio_stream_options_t, num_buffers} ptr (CUInt (soNumBuffers s))
    #{poke rtaudio_stream_options_t, priority} ptr (CInt (soPriority s))
    pokeCStringLen maxNameLength (#{ptr rtaudio_stream_options_t, name} ptr) (soName s)
  peek ptr =
    StreamOptions <$>
      fmap (StreamFlags . unCUInt) (#{peek rtaudio_stream_options_t, flags} ptr) <*>
      fmap unCUInt (#{peek rtaudio_stream_options_t, num_buffers} ptr) <*>
      fmap unCInt (#{peek rtaudio_stream_options_t, priority} ptr) <*>
      peekCString (#{ptr rtaudio_stream_options_t, name} ptr)

newtype FormatInternal = FormatInternal { unFormatInternal :: CUInt } deriving newtype (Storable)

newtype StreamFlagsInternal = StreamFlagsInternal { unStreamFlagsInternal :: CUInt } deriving newtype (Storable)

newtype StreamStatusInternal = StreamStatusInternal { unStreamStatusInternal :: CUInt } deriving newtype (Storable)

type StreamCallbackInternal = Ptr () -> Ptr () -> CUInt -> CDouble -> StreamStatusInternal -> Ptr () -> IO ()

type ErrorCallbackInternal = ErrorCodeInternal -> CString -> IO ()

-- //! Determine the current RtAudio version.  See \ref RtAudio::getVersion().
foreign import ccall "rtaudio_c.h rtaudio_version"
  rtaudio_version :: IO CString

-- //! Determine the number of available compiled audio APIs, the length
-- //! of the array returned by rtaudio_compiled_api().  See \ref
-- //! RtAudio::getCompiledApi().
-- RTAUDIOAPI unsigned int rtaudio_get_num_compiled_apis(void);
foreign import ccall "rtaudio_c.h rtaudio_get_num_compiled_apis"
  rtaudio_get_num_compiled_apis :: IO CInt

-- //! Return an array of rtaudio_api_t compiled into this instance of
-- //! RtAudio.  This array is static (do not free it) and has the length
-- //! returned by rtaudio_get_num_compiled_apis().  See \ref
-- //! RtAudio::getCompiledApi().
-- RTAUDIOAPI const rtaudio_api_t *rtaudio_compiled_api(void);
foreign import ccall "rtaudio_c.h rtaudio_compiled_api"
  rtaudio_compiled_api :: IO (Ptr ApiInternal)

-- //! Return the name of a specified rtaudio_api_t.  This string can be
-- //! used to look up an API by rtaudio_compiled_api_by_name().  See
-- //! \ref RtAudio::getApiName().
-- RTAUDIOAPI const char *rtaudio_api_name(rtaudio_api_t api);
foreign import ccall "rtaudio_c.h rtaudio_api_name"
  rtaudio_api_name :: ApiInternal -> IO CString

-- //! Return the display name of a specified rtaudio_api_t.  See \ref
-- //! RtAudio::getApiDisplayName().
-- RTAUDIOAPI const char *rtaudio_api_display_name(rtaudio_api_t api);
foreign import ccall "rtaudio_c.h rtaudio_api_display_name"
  rtaudio_api_display_name :: ApiInternal -> IO CString

-- //! Return the rtaudio_api_t having the given name.  See \ref
-- //! RtAudio::getCompiledApiByName().
-- RTAUDIOAPI rtaudio_api_t rtaudio_compiled_api_by_name(const char *name);
foreign import ccall "rtaudio_c.h rtaudio_compiled_api_by_name"
  rtaudio_compiled_api_by_name :: CString -> IO ApiInternal

-- RTAUDIOAPI const char *rtaudio_error(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_error"
  rtaudio_error :: Ptr AudioInternal -> IO CString

-- //! Create an instance of struct rtaudio.
-- RTAUDIOAPI rtaudio_t rtaudio_create(rtaudio_api_t api);
foreign import ccall "rtaudio_c.h rtaudio_create"
  rtaudio_create :: ApiInternal -> IO (Ptr AudioInternal)

-- //! Free an instance of struct rtaudio.
-- RTAUDIOAPI void rtaudio_destroy(rtaudio_t audio);
foreign import ccall "rtaudio_c.h &rtaudio_destroy"
  rtaudio_destroy :: FunPtr (Ptr AudioInternal -> IO ())

-- //! Returns the audio API specifier for the current instance of
-- //! RtAudio.  See RtAudio::getCurrentApi().
-- RTAUDIOAPI rtaudio_api_t rtaudio_current_api(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_current_api"
  rtaudio_current_api :: Ptr AudioInternal -> IO ApiInternal

-- //! Queries for the number of audio devices available.  See \ref
-- //! RtAudio::getDeviceCount().
-- RTAUDIOAPI int rtaudio_device_count(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_device_count"
  rtaudio_device_count :: Ptr AudioInternal -> IO CInt

-- //! Return a rtaudio_device_info_t for a specified device number.
-- //! See \ref RtAudio::getDeviceInfo().
-- RTAUDIOAPI rtaudio_device_info_t rtaudio_get_device_info(rtaudio_t audio,
--                                                          int i);
foreign import ccall "wrapper.h wrap_rtaudio_get_device_info"
  rtaudio_get_device_info :: Ptr AudioInternal -> CInt -> Ptr DeviceInfo -> IO ()

-- //! Returns the index of the default output device.  See \ref
-- //! RtAudio::getDefaultOutputDevice().
-- RTAUDIOAPI unsigned int rtaudio_get_default_output_device(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_get_default_output_device"
  rtaudio_get_default_output_device :: Ptr AudioInternal -> IO CUInt

-- //! Returns the index of the default input device.  See \ref
-- //! RtAudio::getDefaultInputDevice().
-- RTAUDIOAPI unsigned int rtaudio_get_default_input_device(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_get_default_input_device"
  rtaudio_get_default_input_device :: Ptr AudioInternal -> IO CUInt

-- //! Opens a stream with the specified parameters.  See \ref RtAudio::openStream().
-- //! \return an \ref rtaudio_error.
-- RTAUDIOAPI int
-- rtaudio_open_stream(rtaudio_t audio, rtaudio_stream_parameters_t *output_params,
--                     rtaudio_stream_parameters_t *input_params,
--                     rtaudio_format_t format, unsigned int sample_rate,
--                     unsigned int *buffer_frames, rtaudio_cb_t cb,
--                     void *userdata, rtaudio_stream_options_t *options,
--                     rtaudio_error_cb_t errcb);
foreign import ccall "rtaudio_c.h rtaudio_open_stream"
  rtaudio_open_stream :: Ptr AudioInternal
                      -> Ptr StreamParams
                      -> Ptr StreamParams
                      -> FormatInternal
                      -> CUInt
                      -> Ptr CUInt
                      -> FunPtr StreamCallbackInternal
                      -> Ptr ()
                      -> Ptr StreamOptions
                      -> FunPtr ErrorCallbackInternal
                      -> IO CUInt

-- //! Closes a stream and frees any associated stream memory.  See \ref RtAudio::closeStream().
-- RTAUDIOAPI void rtaudio_close_stream(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_close_stream"
  rtaudio_close_stream :: Ptr AudioInternal -> IO ()

-- //! Starts a stream.  See \ref RtAudio::startStream().
-- RTAUDIOAPI int rtaudio_start_stream(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_start_stream"
  rtaudio_start_stream :: Ptr AudioInternal -> IO CInt

-- //! Stop a stream, allowing any samples remaining in the output queue
-- //! to be played.  See \ref RtAudio::stopStream().
-- RTAUDIOAPI int rtaudio_stop_stream(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_stop_stream"
  rtaudio_stop_stream :: Ptr AudioInternal -> IO CInt

-- //! Stop a stream, discarding any samples remaining in the
-- //! input/output queue.  See \ref RtAudio::abortStream().
-- RTAUDIOAPI int rtaudio_abort_stream(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_abort_stream"
  rtaudio_abort_stream :: Ptr AudioInternal -> IO CInt

-- //! Returns 1 if a stream is open and false if not.  See \ref RtAudio::isStreamOpen().
-- RTAUDIOAPI int rtaudio_is_stream_open(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_is_stream_open"
  rtaudio_is_stream_open :: Ptr AudioInternal -> IO CInt

-- //! Returns 1 if a stream is running and false if it is stopped or not
-- //! open.  See \ref RtAudio::isStreamRunning().
-- RTAUDIOAPI int rtaudio_is_stream_running(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_is_stream_running"
  rtaudio_is_stream_running :: Ptr AudioInternal -> IO CInt

-- //! Returns the number of elapsed seconds since the stream was
-- //! started.  See \ref RtAudio::getStreamTime().
-- RTAUDIOAPI double rtaudio_get_stream_time(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_get_stream_time"
  rtaudio_get_stream_time :: Ptr AudioInternal -> IO CDouble

-- //! Set the stream time to a time in seconds greater than or equal to
-- //! 0.0.  See \ref RtAudio::setStreamTime().
-- RTAUDIOAPI void rtaudio_set_stream_time(rtaudio_t audio, double time);
foreign import ccall "rtaudio_c.h rtaudio_set_stream_time"
  rtaudio_set_stream_time :: Ptr AudioInternal -> CDouble -> IO ()

-- //! Returns the internal stream latency in sample frames.  See \ref
-- //! RtAudio::getStreamLatency().
-- RTAUDIOAPI int rtaudio_get_stream_latency(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_get_stream_latency"
  rtaudio_get_stream_latency :: Ptr AudioInternal -> IO CInt

-- //! Returns actual sample rate in use by the stream.  See \ref
-- //! RtAudio::getStreamSampleRate().
-- RTAUDIOAPI unsigned int rtaudio_get_stream_sample_rate(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_get_stream_sample_rate"
  rtaudio_get_stream_sample_rate :: Ptr AudioInternal -> IO CUInt

-- //! Specify whether warning messages should be printed to stderr.  See
-- //! \ref RtAudio::showWarnings().
-- RTAUDIOAPI void rtaudio_show_warnings(rtaudio_t audio, int show);
foreign import ccall "rtaudio_c.h rtaudio_show_warnings"
  rtaudio_show_warnings :: Ptr AudioInternal -> CInt -> IO ()
