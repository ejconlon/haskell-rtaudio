{-# LANGUAGE ForeignFunctionInterface #-}

module Sound.RtAudio.Foreign where

import Foreign (Ptr)
import Foreign.C (CInt (..), CString)

-- Will be mapped to Api
type ApiEnum = CInt

-- Opaque `rtaudio` struct
data AudioStruct

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
foreign import ccall "rtaudio_c.h rtaudio_destroy"
  rtaudio_destroy :: Ptr AudioStruct -> IO ()

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
