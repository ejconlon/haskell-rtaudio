{-# LANGUAGE ForeignFunctionInterface #-}

module Sound.RtAudio.Foreign where

import Foreign (FunPtr, Ptr)
import Foreign.C (CDouble (..), CInt (..), CString (..), CUInt (..))

-- Will be mapped to Api
type ApiEnum = CInt

-- Will be mapped to ErrorCode
type ErrorCodeEnum = CInt

-- Opaque `rtaudio` struct
data AudioStruct

-- TODO(ejconlon) Fill in
data DeviceInfoStruct

type StreamStatusFlag = CUInt

type StreamCallback = Ptr () -> Ptr () -> CUInt -> CDouble -> StreamStatusFlag -> Ptr () -> IO ()

type ErrorCallback = ErrorCodeEnum -> CString -> IO ()

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
  rtaudio_compiled_api :: IO (Ptr ApiEnum)

-- //! Return the name of a specified rtaudio_api_t.  This string can be
-- //! used to look up an API by rtaudio_compiled_api_by_name().  See
-- //! \ref RtAudio::getApiName().
-- RTAUDIOAPI const char *rtaudio_api_name(rtaudio_api_t api);
foreign import ccall "rtaudio_c.h rtaudio_api_name"
  rtaudio_api_name :: ApiEnum -> IO CString

-- //! Return the display name of a specified rtaudio_api_t.  See \ref
-- //! RtAudio::getApiDisplayName().
-- RTAUDIOAPI const char *rtaudio_api_display_name(rtaudio_api_t api);
foreign import ccall "rtaudio_c.h rtaudio_api_display_name"
  rtaudio_api_display_name :: ApiEnum -> IO CString

-- //! Return the rtaudio_api_t having the given name.  See \ref
-- //! RtAudio::getCompiledApiByName().
-- RTAUDIOAPI rtaudio_api_t rtaudio_compiled_api_by_name(const char *name);
foreign import ccall "rtaudio_c.h rtaudio_compiled_api_by_name"
  rtaudio_compiled_api_by_name :: CString -> IO ApiEnum

-- RTAUDIOAPI const char *rtaudio_error(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_error"
  rtaudio_error :: Ptr AudioStruct -> IO CString

-- //! Create an instance of struct rtaudio.
-- RTAUDIOAPI rtaudio_t rtaudio_create(rtaudio_api_t api);
foreign import ccall "rtaudio_c.h rtaudio_create"
  rtaudio_create :: ApiEnum -> IO (Ptr AudioStruct)

-- //! Free an instance of struct rtaudio.
-- RTAUDIOAPI void rtaudio_destroy(rtaudio_t audio);
foreign import ccall "rtaudio_c.h &rtaudio_destroy"
  rtaudio_destroy :: FunPtr (Ptr AudioStruct -> IO ())

-- //! Returns the audio API specifier for the current instance of
-- //! RtAudio.  See RtAudio::getCurrentApi().
-- RTAUDIOAPI rtaudio_api_t rtaudio_current_api(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_current_api"
  rtaudio_current_api :: Ptr AudioStruct -> IO ApiEnum

-- //! Queries for the number of audio devices available.  See \ref
-- //! RtAudio::getDeviceCount().
-- RTAUDIOAPI int rtaudio_device_count(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_device_count"
  rtaudio_device_count :: Ptr AudioStruct -> IO CInt

-- //! Return a struct rtaudio_device_info for a specified device number.
-- //! See \ref RtAudio::getDeviceInfo().
-- RTAUDIOAPI rtaudio_device_info_t rtaudio_get_device_info(rtaudio_t audio,
--                                                          int i);
-- foreign import ccall "rtaudio_c.h rtaudio_get_device_info"
--   rtaudio_get_device_info :: Ptr AudioStruct -> CInt -> IO DeviceInfoStruct

-- //! Returns the index of the default output device.  See \ref
-- //! RtAudio::getDefaultOutputDevice().
-- RTAUDIOAPI unsigned int rtaudio_get_default_output_device(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_get_default_output_device"
  rtaudio_get_default_output_device :: Ptr AudioStruct -> IO CUInt

-- //! Returns the index of the default input device.  See \ref
-- //! RtAudio::getDefaultInputDevice().
-- RTAUDIOAPI unsigned int rtaudio_get_default_input_device(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_get_default_input_device"
  rtaudio_get_default_input_device :: Ptr AudioStruct -> IO CUInt

-- //! Opens a stream with the specified parameters.  See \ref RtAudio::openStream().
-- //! \return an \ref rtaudio_error.
-- RTAUDIOAPI int
-- rtaudio_open_stream(rtaudio_t audio, rtaudio_stream_parameters_t *output_params,
--                     rtaudio_stream_parameters_t *input_params,
--                     rtaudio_format_t format, unsigned int sample_rate,
--                     unsigned int *buffer_frames, rtaudio_cb_t cb,
--                     void *userdata, rtaudio_stream_options_t *options,
--                     rtaudio_error_cb_t errcb);

-- //! Closes a stream and frees any associated stream memory.  See \ref RtAudio::closeStream().
-- RTAUDIOAPI void rtaudio_close_stream(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_close_stream"
  rtaudio_close_stream :: Ptr AudioStruct -> IO ()

-- //! Starts a stream.  See \ref RtAudio::startStream().
-- RTAUDIOAPI int rtaudio_start_stream(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_start_stream"
  rtaudio_start_stream :: Ptr AudioStruct -> IO CInt

-- //! Stop a stream, allowing any samples remaining in the output queue
-- //! to be played.  See \ref RtAudio::stopStream().
-- RTAUDIOAPI int rtaudio_stop_stream(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_stop_stream"
  rtaudio_stop_stream :: Ptr AudioStruct -> IO CInt

-- //! Stop a stream, discarding any samples remaining in the
-- //! input/output queue.  See \ref RtAudio::abortStream().
-- RTAUDIOAPI int rtaudio_abort_stream(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_abort_stream"
  rtaudio_abort_stream :: Ptr AudioStruct -> IO CInt

-- //! Returns 1 if a stream is open and false if not.  See \ref RtAudio::isStreamOpen().
-- RTAUDIOAPI int rtaudio_is_stream_open(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_is_stream_open"
  rtaudio_is_stream_open :: Ptr AudioStruct -> IO CInt

-- //! Returns 1 if a stream is running and false if it is stopped or not
-- //! open.  See \ref RtAudio::isStreamRunning().
-- RTAUDIOAPI int rtaudio_is_stream_running(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_is_stream_running"
  rtaudio_is_stream_running :: Ptr AudioStruct -> IO CInt

-- //! Returns the number of elapsed seconds since the stream was
-- //! started.  See \ref RtAudio::getStreamTime().
-- RTAUDIOAPI double rtaudio_get_stream_time(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_get_stream_time"
  rtaudio_get_stream_time :: Ptr AudioStruct -> IO CDouble

-- //! Set the stream time to a time in seconds greater than or equal to
-- //! 0.0.  See \ref RtAudio::setStreamTime().
-- RTAUDIOAPI void rtaudio_set_stream_time(rtaudio_t audio, double time);
foreign import ccall "rtaudio_c.h rtaudio_set_stream_time"
  rtaudio_set_stream_time :: Ptr AudioStruct -> CDouble -> IO ()

-- //! Returns the internal stream latency in sample frames.  See \ref
-- //! RtAudio::getStreamLatency().
-- RTAUDIOAPI int rtaudio_get_stream_latency(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_get_stream_latency"
  rtaudio_get_stream_latency :: Ptr AudioStruct -> IO CInt

-- //! Returns actual sample rate in use by the stream.  See \ref
-- //! RtAudio::getStreamSampleRate().
-- RTAUDIOAPI unsigned int rtaudio_get_stream_sample_rate(rtaudio_t audio);
foreign import ccall "rtaudio_c.h rtaudio_get_stream_sample_rate"
  rtaudio_get_stream_sample_rate :: Ptr AudioStruct -> IO CUInt

-- //! Specify whether warning messages should be printed to stderr.  See
-- //! \ref RtAudio::showWarnings().
-- RTAUDIOAPI void rtaudio_show_warnings(rtaudio_t audio, int show);
foreign import ccall "rtaudio_c.h rtaudio_show_warnings"
  rtaudio_show_warnings :: Ptr AudioStruct -> CInt -> IO ()
