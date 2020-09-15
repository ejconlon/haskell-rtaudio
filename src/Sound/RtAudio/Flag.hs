module Sound.RtAudio.Flag
  ( BitFlag (..)
  , Flag (..)
  ) where

import Data.Bits (Bits, zeroBits, (.&.), (.|.))

-- | Flags in the C sense are monoids that act like sets.
class Monoid a => Flag a where
  matchFlag :: a -> a -> Bool

-- | A 'Flag' that works by combining values with bitwise-or, and matching them with bitwise-and.
newtype BitFlag a = BitFlag { unBitFlag :: a } deriving (Eq, Show)

instance Bits a => Semigroup (BitFlag a) where
  (BitFlag x) <> (BitFlag y) = BitFlag (x .|. y)

instance Bits a => Monoid (BitFlag a) where
  mempty = BitFlag zeroBits
  mappend = (<>)

instance Bits a => Flag (BitFlag a) where
  matchFlag (BitFlag x) (BitFlag y) = x .&. y /= zeroBits
