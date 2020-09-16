{-# LANGUAGE FlexibleContexts #-}

module Sound.RtAudio.Flag
  ( BitFlagSet (..)
  , FlagSet (..)
  , FlagValue (..)
  , enumerateFlagValues
  ) where

import Data.Bits (Bits, zeroBits, (.&.), (.|.))

-- | Flags in the C sense are monoids that act like sets.
class Monoid a => FlagSet a where
  matchFlagSet :: a -> a -> Bool

-- | Flag values are enums with associated 'FlagSet' types.
class FlagSet (AssocFlagSet b) => FlagValue b where
  type AssocFlagSet b
  matchFlagValue :: AssocFlagSet b -> b -> Bool
  matchFlagValue a = matchFlagSet a . toFlagSet
  toFlagSet :: b -> AssocFlagSet b

enumerateFlagValues :: (Bounded b, Enum b, FlagValue b) => AssocFlagSet b -> [b]
enumerateFlagValues s = [b | b <- enumFromTo minBound maxBound, matchFlagValue s b]

-- | A 'Flag' that works by combining values with bitwise-or, and matching them with bitwise-and.
newtype BitFlagSet a = BitFlagSet { unBitFlagSet :: a } deriving stock (Eq, Show)

instance Bits a => Semigroup (BitFlagSet a) where
  (BitFlagSet x) <> (BitFlagSet y) = BitFlagSet (x .|. y)

instance Bits a => Monoid (BitFlagSet a) where
  mempty = BitFlagSet zeroBits
  mappend = (<>)

instance Bits a => FlagSet (BitFlagSet a) where
  matchFlagSet (BitFlagSet x) (BitFlagSet y) = x .&. y /= zeroBits
