module Sound.RtAudio
  ( Api (..)
  , Audio
  , StreamCallback
  , InputStreamCallback
  , OutputStreamCallback
  , DuplexStreamCallback
  , ErrorCallback
  , Error (..)
  , Format (..)
  , DeviceInfo (..)
  , StreamParams (..)
  , StreamOptions (..)
  , FormatSet
  , FormatValue (..)
  , StreamStatusSet
  , StreamStatusValue (..)
  , StreamFlagsSet
  , StreamFlagsValue (..)
  , formatValue
  , getVersion
  , getCompiledApis
  , apiName
  , apiDisplayName
  , apiByName
  , createAudio
  , currentApi
  , deviceCount
  , getDeviceInfo
  , getDefaultOutputDevice
  , getDefaultInputDevice
  , openInputStream
  , openOutputStream
  , openDuplexStream
  , closeStream
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Data.Bits ((.&.))
import Data.Int (Int16, Int32, Int8)
import Data.Int.Int24 (Int24)
import Data.Proxy (Proxy (..))
import Foreign (Ptr, Storable (..), alloca, nullPtr, peek, peekArray)
import Foreign.C (CInt (..), peekCString, withCString)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Sound.RtAudio.Buffers
import Sound.RtAudio.Foreign

-- | An internal RtAudio error
newtype Error = Error String deriving stock (Eq, Show)
instance Exception Error

newtype Audio = Audio { unAudio :: ForeignPtr AudioInternal } deriving stock (Eq, Show)

type StreamCallback m e = Double -> StreamStatusSet -> e -> m ()

type InputStreamCallback m a = StreamCallback m (InputBuffer a)

type OutputStreamCallback m a = StreamCallback m (OutputBuffer a)

type DuplexStreamCallback m a = StreamCallback m (DuplexBuffer a)

type ErrorCallback m = ErrorCode -> String -> m ()

data Format a where
  FormatInt8 :: Format Int8
  FormatInt16 :: Format Int16
  FormatInt24 :: Format Int24
  FormatInt32 :: Format Int32
  FormatFloat32 :: Format Float
  FormatFloat64 :: Format Double

deriving stock instance Eq (Format a)
deriving stock instance Show (Format a)

formatValue :: Format a -> FormatValue
formatValue f =
  case f of
    FormatInt8 -> FormatValueInt8
    FormatInt16 -> FormatValueInt16
    FormatInt24 -> FormatValueInt24
    FormatInt32 -> FormatValueInt32
    FormatFloat32 -> FormatValueFloat32
    FormatFloat64 -> FormatValueFloat64

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
getVersion :: MonadIO m => m String
getVersion = liftIO (rtaudio_version >>= peekCString)

-- | Returns a list of all supported RtMidi APIs.
getCompiledApis :: MonadIO m => m [Api]
getCompiledApis = liftIO $ do
  n <- rtaudio_get_num_compiled_apis
  ptr <- rtaudio_compiled_api
  arr <- peekArray (fromIntegral n) ptr
  pure (fmap toApi arr)

-- | Returns the internal name of the RtMidi API.
apiName :: MonadIO m => Api -> m String
apiName api = liftIO (rtaudio_api_name (fromApi api) >>= peekCString)

-- | Returns the display name of the RtMidi API.
apiDisplayName :: MonadIO m => Api -> m String
apiDisplayName api = liftIO (rtaudio_api_display_name (fromApi api) >>= peekCString)

-- | Looks up the RtMidi API by its /internal/ name.
--
-- Note that this is the name from 'apiName', not 'apiDisplayName'.
-- If not found, returns 'UnspecifiedApi'.
apiByName :: MonadIO m => String -> m Api
apiByName name = liftIO (fmap toApi (withCString name rtaudio_compiled_api_by_name))

-- | Creates an 'Audio' interface using the given API, or throws an error.
createAudio :: MonadIO m => Api -> m Audio
createAudio api = liftIO $ do
  ast <- rtaudio_create (fromApi api)
  fptr <- newForeignPtr rtaudio_destroy ast
  guardError ast
  pure (Audio fptr)

-- | Returns the API used by the 'Audio' interface.
currentApi :: MonadIO m => Audio -> m Api
currentApi = liftIO . flip withAudioStructUnguarded (fmap toApi . rtaudio_current_api)

-- | Returns the number of devices available to the 'Audio' interface.
deviceCount :: MonadIO m => Audio -> m Int
deviceCount = liftIO . flip withAudioStructUnguarded (fmap fromIntegral . rtaudio_device_count)

-- | Returns 'DeviceInfo' for the given audio device.
-- The index must be less than or equal to the count returned by 'audioDeviceCount',
-- otherwise it throws an 'Error'.
getDeviceInfo :: MonadIO m => Audio -> Int -> m DeviceInfo
getDeviceInfo audio index = liftIO $ withAudioStruct audio $ \ptr -> alloca $ \dptr -> do
  rtaudio_get_device_info ptr (fromIntegral index) dptr
  peek dptr

getDefaultOutputDevice :: MonadIO m => Audio -> m Int
getDefaultOutputDevice audio = liftIO $ withAudioStruct audio $ \ptr ->
  fmap fromIntegral (rtaudio_get_default_output_device ptr)

getDefaultInputDevice :: MonadIO m => Audio -> m Int
getDefaultInputDevice audio = liftIO $ withAudioStruct audio $ \ptr ->
  fmap fromIntegral (rtaudio_get_default_output_device ptr)

openInputStream :: (MonadUnliftIO m, Storable a) => Audio -> Int -> Format a -> InputStreamCallback m a -> ErrorCallback m -> m ()
openInputStream = openStream Proxy

openOutputStream :: (MonadUnliftIO m, Storable a) => Audio -> Int -> Format a -> OutputStreamCallback m a -> ErrorCallback m -> m ()
openOutputStream = openStream Proxy

openDuplexStream :: (MonadUnliftIO m, Storable a) => Audio -> Int -> Format a -> DuplexStreamCallback m a -> ErrorCallback m -> m ()
openDuplexStream = openStream Proxy

openStream :: (MonadUnliftIO m, ReflectBuffer b, Storable a) => Proxy (b a) -> Audio -> Int -> Format a -> StreamCallback m (b a) -> ErrorCallback m -> m ()
openStream proxy audio index fmt streamCb errCb = do
  let bufferType = reflectBufferType proxy
  error "TODO"

closeStream :: MonadIO m => Audio -> m ()
closeStream = liftIO . flip withAudioStruct rtaudio_close_stream
