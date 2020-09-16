{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Sound.RtAudio.Buffers
  ( Buffer (..)
  , InputBuffer (..)
  , foreignInputBuffer
  , readInputBuffer
  , OutputBuffer (..)
  , foreignOutputBuffer
  , writeOutputBuffer
  , genOutputBuffer
  , genOutputBufferIO
  , copyBuffer
  , DuplexBuffer (..)
  , foreignDuplexBuffer
  , copyDuplexBuffer
  ) where

import Control.DeepSeq (NFData)
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign (Storable (..), ForeignPtr, Ptr, nullPtr)
import GHC.Generics (Generic)

proxyUndefined :: f a -> a
proxyUndefined = undefined

class Buffer b where
  newBuffer :: Storable a => Int -> IO (b a)
  bufferByteSize :: Storable a => b a -> Int

instance Buffer VSM.IOVector where
  newBuffer = VSM.new
  bufferByteSize v = sizeOf (proxyUndefined v) * VSM.length v

newtype InputBuffer a = InputBuffer { unInputBuffer :: VSM.IOVector a } deriving newtype (Buffer, NFData)

foreignInputBuffer :: Storable a => ForeignPtr a -> Int -> InputBuffer a
foreignInputBuffer fptr = InputBuffer . VSM.unsafeFromForeignPtr0 fptr

readInputBuffer :: Storable a => InputBuffer a -> Int -> IO a
readInputBuffer = undefined

newtype OutputBuffer a = OutputBuffer { unOutputBuffer :: VSM.IOVector a } deriving newtype (Buffer, NFData)

foreignOutputBuffer :: Storable a => ForeignPtr a -> Int -> OutputBuffer a
foreignOutputBuffer fptr = OutputBuffer . VSM.unsafeFromForeignPtr0 fptr

writeOutputBuffer :: Storable a => OutputBuffer a -> Int -> a -> IO ()
writeOutputBuffer = undefined

genOutputBuffer :: Storable a => OutputBuffer a -> (Int -> a) -> IO ()
genOutputBuffer = undefined

genOutputBufferIO :: Storable a => OutputBuffer a -> (Int -> IO a) -> IO ()
genOutputBufferIO = undefined

copyBuffer :: Storable a => OutputBuffer a -> InputBuffer a -> IO ()
copyBuffer = undefined

data DuplexBuffer a = DuplexBuffer
  { duplexBufferOutput :: !(OutputBuffer a)
  , duplexBufferInput :: !(InputBuffer a)
  } deriving stock (Generic)
    deriving anyclass (NFData)

instance Buffer DuplexBuffer where
  newBuffer sz = DuplexBuffer <$> newBuffer sz <*> newBuffer sz
  bufferByteSize (DuplexBuffer o _) = bufferByteSize o

foreignDuplexBuffer :: Storable a => ForeignPtr a -> ForeignPtr a -> Int -> DuplexBuffer a
foreignDuplexBuffer = undefined

copyDuplexBuffer :: Storable a => DuplexBuffer a -> IO ()
copyDuplexBuffer (DuplexBuffer o i) = copyBuffer o i
