{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Concepts largely taken from Graphics.UI.WX.Attributes
module Reflex.Cocos2d.Attributes
  (
    Attrib(..)
  , attrib
  , SetOnlyAttrib(..)
  , Prop(..)
  , get
  , set
  , dyn
  , dyn'
  , evt
  -- attrs --
  , HasPosition(..)
  , HasRotation(..)
  ) where

import Diagrams (P2, (^&), unp2, _x, _y, Direction, V2)
import Control.Lens hiding (set)
import Control.Monad
import Control.Monad.IO.Class
import Reflex
import Reflex.Host.Class

import Reflex.Cocos2d.Class

infixr 0 :=,:~

data Prop w m = forall a c. IsSettable w m a (c w m a) => (c w m a) := a -- ^ Assign a value to an attribute
              | forall a c. IsUpdateable w m a (c w m a) => (c w m a) :~ (a -> a)  -- ^ Apply an update function to an attribute

class IsSettable w m a attr | attr -> w m a where
  setter :: attr -> w -> a -> m ()

class IsGettable w m a attr | attr -> w m a where
  getter :: attr -> w -> m a

class IsUpdateable w m a attr | attr -> w m a where
  updater :: attr -> w -> (a -> a) -> m a

-- | a datatype that supports getter, setter, and updater
data Attrib w m a = Attrib (w -> m a) -- ^ Getter
                           (w -> a -> m ()) -- ^ Setter
                           (w -> (a -> a) -> m a) -- ^ Updater

instance IsSettable w m a (Attrib w m a) where
    setter (Attrib _ s _) = s

instance IsGettable w m a (Attrib w m a) where
    getter (Attrib g _ _) = g

instance IsUpdateable w m a (Attrib w m a) where
    updater (Attrib _ _ u) = u


data SetOnlyAttrib w m a = SetOnlyAttrib (w -> a -> m ())

instance IsSettable w m a (SetOnlyAttrib w m a) where
    setter (SetOnlyAttrib s) = s

get :: IsGettable n m a attr => n -> attr -> m a
get = flip getter

set :: Monad m => n -> [Prop n m] -> m ()
set _ [] = return ()
set n ((s := a):ps) = setter s n a >> set n ps
set n ((u :~ f):ps) = updater u n f >> set n ps

-- | Transforms a IsSettable attribute to a SetOnlyAttribute. This uses lazy read on the incoming Dynamic
dyn :: (NodeGraph t m, IsSettable w (HostFrame t) a (attr w (HostFrame t) a))
    => attr w (HostFrame t) a -> SetOnlyAttrib w m (Dynamic t a)
dyn attr = SetOnlyAttrib $ \w d -> do schedulePostBuild $ setter attr w =<< sample (current d)
                                      es w (updated d)
  where SetOnlyAttrib es = evt attr

-- | Similar to `dyn`, but applies strict read
-- XXX: nasty constraints to allow 'c' to be instantiated as different types within the function
dyn' :: ( NodeGraph t m
        , IsSettable w (HostFrame t) a (attr w (HostFrame t) a)
        , IsSettable w m a (attr w m a) )
     => (forall m'. (MonadIO m', IsSettable w m' a (attr w m' a)) => attr w m' a)
     -> SetOnlyAttrib w m (Dynamic t a)
dyn' attr = SetOnlyAttrib $ \w d -> do setter attr w =<< sample (current d)
                                       es w (updated d)
  where SetOnlyAttrib es = evt attr

evt :: (NodeGraph t m, IsSettable w (HostFrame t) a (attr w (HostFrame t) a))
    => attr w (HostFrame t) a -> SetOnlyAttrib w m (Event t a)
evt attr = SetOnlyAttrib $ \w e -> sequenceH_ . ffor e $ setter attr w

attrib :: Monad m => (w -> m a) -> (w -> a -> m ()) -> Attrib w m a
attrib getter setter = Attrib getter setter updater
  where updater w u = do
          a <- getter w
          let a' = u a
          setter w a'
          return a'

---- General Attribs ----

class Monad m => HasPosition n m where
  pos :: Attrib n m (P2 Double)
  pos = attrib (\n -> (^&) <$> gx n <*> gy n)
               (\n p -> let (x, y) = unp2 p in sx n x >> sy n y)
    where Attrib gx sx _ = posX
          Attrib gy sy _ = posY
  posX :: Attrib n m Double
  posX = Attrib getter' setter' (\n u -> (^._x) <$> updater n (_x %~ u))
    where Attrib getter _ updater = pos
          getter' n = (^._x) <$> getter n
          setter' n x = void $ updater n (_x .~ x)
  posY :: Attrib n m Double
  posY = Attrib getter' setter' (\n u -> (^._y) <$> updater n (_y %~ u))
    where Attrib getter _ updater = pos
          getter' n = (^._y) <$> getter n
          setter' n y = void $ updater n (_y .~ y)

class HasRotation n m where
    rot :: Attrib n m (Direction V2 Double)
