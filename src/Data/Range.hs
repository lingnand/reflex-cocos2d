{-# LANGUAGE DefaultSignatures #-}
module Data.Range where

import Data.Int
import Data.Word
import Linear

-- | The Range class generalizes the @inRange@ function of the Ix class
-- (extending from discrete domain to continuous)
class Range a where
    inRange :: (a, a) -> a -> Bool
    limit :: (a, a) -> a -> a

    default inRange :: Ord a => (a, a) -> a -> Bool
    inRange (l, u) a = a >= l && a <= u
    default limit :: Ord a => (a, a) -> a -> a
    limit (l, u) a = min u $ max l a

instance Range Char where
instance Range Int where
instance Range Int8 where
instance Range Int16 where
instance Range Int32 where
instance Range Int64 where
instance Range Word where
instance Range Word16 where
instance Range Word32 where
instance Range Word64 where
instance Range Double where
instance Range Float where

instance Range a => Range (V2 a) where
    inRange (V2 lx ly, V2 ux uy) (V2 x y) = inRange (lx, ux) x && inRange (ly, uy) y
    limit (V2 lx ly, V2 ux uy) (V2 x y) = V2 (limit (lx, ux) x) (limit (ly, uy) y)
