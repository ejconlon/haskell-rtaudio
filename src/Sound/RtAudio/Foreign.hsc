{-# LANGUAGE ForeignFunctionInterface #-}

module Sound.RtAudio.Foreign where

import Foreign.C (CString)

foreign import ccall "rtaudio_c.h rtaudio_version"
  rtaudio_version :: IO CString
