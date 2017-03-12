{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
-- A simple structure for a pool of resources.
-- Each managed resource has an integer index that identifies the 'slot' for the given resource;
-- when the resource is idle, this slot holds a Nothing; when it's currently used it holds the
-- content of the resource
module Data.Pool
  (
    Pool
  , Id
  -- * Accessors
  , busy
  , idles
  , slots
  -- * Query
  , null
  , size
  -- * Creation
  , empty
  , idling
  , fromBusy
  -- * Modification
  , takeSlot
  , takeSlot_
  , takeSlots
  , takeSlots_
  , releaseSlot
  , reserve
  , growBy
  ) where

import Prelude hiding (null)
import Data.Tuple (swap)
import Data.List (mapAccumL)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

type Id = Int

-- elems hold the elements in use
-- idles contains the currently unoccupied ids (but issued in the past)
-- nexts contains the lazy list of all the future ids to issue
-- therefore, keysSet elems \cap idles = \0
data Pool a = Pool
    (IM.IntMap a)  -- the busy / occupied cells
    IS.IntSet      -- the idle cells
    Id             -- the base of the remaining id
  deriving (Show, Eq, Ord)

-- TODO: Show instance, Eq instance..?

busy :: Pool a -> IM.IntMap a
busy (Pool busy _ _) = busy

idles :: Pool a -> IS.IntSet
idles (Pool _ idles _) = idles

-- all the slots in the pool, those busy contain a 'Just a'; whereas the idle ones have a Nothing
slots :: Pool a -> IM.IntMap (Maybe a)
slots (Pool busy idles _) = IM.union (Just <$> busy) (IM.fromSet (const Nothing) idles)

null :: Pool a -> Bool
null = (&&) <$> IM.null . busy <*> IS.null . idles

size :: Pool a -> Int
size = (+) <$> IM.size . busy <*> IS.size . idles

empty :: Pool a
empty = Pool IM.empty IS.empty 0

idling :: Int -> Pool a
idling c = Pool IM.empty (IS.fromList [0..c-1]) c

-- | call takeSlots on the elements with an empty starting pool
fromBusy :: [a] -> Pool a
fromBusy = flip takeSlots_ empty

takeSlot_ :: a -> Pool a -> Pool a
takeSlot_ a = snd . takeSlot a

takeSlot :: a -> Pool a -> (Id, Pool a)
takeSlot a (Pool busy idles next)
  | IS.null idles = (next, Pool (IM.insert next a busy) idles (next+1))
  | otherwise =
    let (k, idles') = IS.deleteFindMin idles
    in (k, Pool (IM.insert k a busy) idles' next)

-- insert all elements into the pool, starting from the left
takeSlots :: [a] -> Pool a -> ([Id], Pool a)
takeSlots l p = swap $ mapAccumL (\p a -> let (k, p') = takeSlot a p in (p', k)) p l

takeSlots_ :: [a] -> Pool a -> Pool a
takeSlots_ l = snd . takeSlots l

-- | reserve a certain minimum size
reserve :: Int -> Pool a -> Pool a
reserve c p
    | left > 0  = growBy left p
    | otherwise = p
  where left = c - size p

growBy :: Int -> Pool a -> Pool a
growBy s (Pool busy idles next) = Pool busy (foldr IS.insert idles $ take s [next..]) (next+s)

releaseSlot :: Id -> Pool a -> Pool a
releaseSlot k p@(Pool busy idles next)
  | IM.member k busy = Pool (IM.delete k busy) (IS.insert k idles) next
  | otherwise = p
