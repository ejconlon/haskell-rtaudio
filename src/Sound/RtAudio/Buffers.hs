{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Sound.RtAudio.Buffers
  ( Buffer (..)
  , InputBuffer
  , foreignInputBuffer
  , readInputBuffer
  , dumpInputBuffer
  , OutputBuffer
  , foreignOutputBuffer
  , writeOutputBuffer
  , setOutputBuffer
  , genOutputBuffer
  , genOutputBufferIO
  , loadOutputBuffer
  , copyBuffer
  , DuplexBuffer
  , duplexBufferOutput
  , duplexBufferInput
  , foreignDuplexBuffer
  , copyDuplexBuffer
  ) where

import Control.DeepSeq (NFData)
import Data.Foldable (for_)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign (Storable (..), ForeignPtr, Ptr, nullPtr)
import GHC.Generics (Generic)

-- This just makes types line up below
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
readInputBuffer = VSM.read . unInputBuffer

dumpInputBuffer :: Storable a => InputBuffer a -> IO (VS.Vector a)
dumpInputBuffer = VS.freeze . unInputBuffer

newtype OutputBuffer a = OutputBuffer { unOutputBuffer :: VSM.IOVector a } deriving newtype (Buffer, NFData)

foreignOutputBuffer :: Storable a => ForeignPtr a -> Int -> OutputBuffer a
foreignOutputBuffer fptr = OutputBuffer . VSM.unsafeFromForeignPtr0 fptr

writeOutputBuffer :: Storable a => OutputBuffer a -> Int -> a -> IO ()
writeOutputBuffer = VSM.write . unOutputBuffer

setOutputBuffer :: Storable a => OutputBuffer a -> a -> IO ()
setOutputBuffer = VSM.set . unOutputBuffer

genOutputBuffer :: Storable a => OutputBuffer a -> (Int -> a) -> IO ()
genOutputBuffer (OutputBuffer ob) f = for_ [0 .. VSM.length ob - 1] (\i -> VSM.unsafeWrite ob i (f i))

genOutputBufferIO :: Storable a => OutputBuffer a -> (Int -> IO a) -> IO ()
genOutputBufferIO (OutputBuffer ob) f = for_ [0 .. VSM.length ob - 1] (\i -> f i >>= VSM.unsafeWrite ob i)

loadOutputBuffer :: Storable a => InputBuffer a -> VS.Vector a -> IO ()
loadOutputBuffer = VS.copy . unInputBuffer

copyBuffer :: Storable a => OutputBuffer a -> InputBuffer a -> IO ()
copyBuffer (OutputBuffer ob) (InputBuffer ib) = VSM.copy ob ib

data DuplexBuffer a = DuplexBuffer
  { duplexBufferOutput :: !(OutputBuffer a)
  , duplexBufferInput :: !(InputBuffer a)
  } deriving stock (Generic)
    deriving anyclass (NFData)

instance Buffer DuplexBuffer where
  newBuffer sz = DuplexBuffer <$> newBuffer sz <*> newBuffer sz
  bufferByteSize (DuplexBuffer o _) = bufferByteSize o

foreignDuplexBuffer :: Storable a => ForeignPtr a -> ForeignPtr a -> Int -> DuplexBuffer a
foreignDuplexBuffer op ip len = DuplexBuffer (foreignOutputBuffer op len) (foreignInputBuffer ip len)

copyDuplexBuffer :: Storable a => DuplexBuffer a -> IO ()
copyDuplexBuffer (DuplexBuffer o i) = copyBuffer o i
