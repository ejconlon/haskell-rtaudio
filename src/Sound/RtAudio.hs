module Sound.RtAudio where

import Foreign (peekArray)
import Foreign.C (CInt (..), peekCString)
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

getVersion :: IO String
getVersion = rtaudio_version >>= peekCString

-- TODO(ejconlon) This might be platform specific...
enumByteSize :: Int
enumByteSize = 4

getCompiledApis :: IO [Api]
getCompiledApis = do
  n <- rtaudio_get_num_compiled_apis
  ptr <- rtaudio_compiled_api
  arr <- peekArray (fromIntegral n) ptr
  pure (fmap (toEnum . fromIntegral) arr)
