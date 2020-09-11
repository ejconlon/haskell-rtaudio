module Sound.RtAudio where

import Sound.RtAudio.Foreign
import Foreign.C (peekCString)

getVersion :: IO String
getVersion = rtaudio_version >>= peekCString
