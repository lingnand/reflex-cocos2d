{-# LANGUAGE TemplateHaskell #-}
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
    Attrib'(..)
  , Attrib
  , attrib
  , SetOnlyAttrib'(..)
  , SetOnlyAttrib
  , Prop(..)
  , get
  , set
  , dyn
  , dyn'
  , evt
  , divide
  , divided
  , choose
  , chosen
  -- attrs --
  , HasPosition(..)
  , HasRotation(..)
  , Transform(Transform)
  , HasPos(..)
  , HasRot(..)
  , transform
  ) where

import Data.Functor.Contravariant
import Diagrams (P2, (^&), unp2, _x, _y, Direction, V2)
import Control.Lens hiding (set, chosen, transform)
import Control.Monad
import Control.Monad.IO.Class
import Reflex.Host.Class
import Reflex
import Reflex.Cocos2d.Class

infixr 0 :=,:~

data Prop w m = forall a b c. IsSettable w m a (c w m b a) => (c w m b a) := a -- ^ Assign a value to an attribute
              | forall a b c. IsUpdateable w m b a (c w m b a) => (c w m b a) :~ (b -> a)  -- ^ Apply an update function to an attribute

class IsSettable w m a attr | attr -> w m a where
  setter :: attr -> w -> a -> m ()

class IsGettable w m a attr | attr -> w m a where
  getter :: attr -> w -> m a

class IsUpdateable w m b a attr | attr -> w m b a where
  updater :: attr -> w -> (b -> a) -> m b

-- | A datatype that supports getter, setter, and updater
data Attrib' w m b a = Attrib' (w -> m b) -- ^ Getter
                               (w -> a -> m ()) -- ^ Setter
                               (w -> (b -> a) -> m b) -- ^ Updater

type Attrib w m a = Attrib' w m a a

instance IsSettable w m a (Attrib' w m b a) where
    setter (Attrib' _ s _) = s

instance IsGettable w m b (Attrib' w m b a) where
    getter (Attrib' g _ _) = g

instance IsUpdateable w m b a (Attrib' w m b a) where
    updater (Attrib' _ _ u) = u

instance Contravariant (Attrib' w m b) where
    contramap f (Attrib' g s u) = Attrib' g s' u'
      where s' w = s w . f
            u' w uf = u w (f . uf)

attrib :: Monad m => (w -> m a) -> (w -> a -> m ()) -> Attrib w m a
attrib getter setter = Attrib' getter setter updater
  where updater w u = do
          a <- getter w
          let a' = u a
          setter w a'
          return a'

data SetOnlyAttrib' w m b a = SetOnlyAttrib' (w -> a -> m ())

type SetOnlyAttrib w m a = forall b. SetOnlyAttrib' w m b a

instance IsSettable w m a (SetOnlyAttrib' w m b a) where
    setter (SetOnlyAttrib' s) = s

instance Contravariant (SetOnlyAttrib' w m b) where
    contramap f s = SetOnlyAttrib' $ \w -> setter s w . f

get :: IsGettable n m a attr => n -> attr -> m a
get = flip getter

set :: Monad m => n -> [Prop n m] -> m ()
set _ [] = return ()
set n ((s := a):ps) = setter s n a >> set n ps
set n ((u :~ f):ps) = updater u n f >> set n ps

---- combinators ----

-- | Transforms a IsSettable attribute to a SetOnlyAttribute. This uses lazy read on the incoming Dynamic
dyn :: (NodeGraph t m, IsSettable w (HostFrame t) a (attr w (HostFrame t) b a))
    => attr w (HostFrame t) b a -> SetOnlyAttrib w m (Dynamic t a)
dyn attr = SetOnlyAttrib' $ \w d -> do evt <- askPostBuildEvent
                                       runEvent_ $ (setter attr w =<< sample (current d)) <$ evt
                                       es w (updated d)
  where SetOnlyAttrib' es = evt attr

-- | Similar to `dyn`, but applies strict read
-- XXX: nasty constraints to allow 'c' to be instantiated as different types within the function
dyn' :: ( NodeGraph t m
        , IsSettable w (HostFrame t) a (attr w (HostFrame t) b a)
        , IsSettable w m a (attr w m b a) )
     => (forall m'. (MonadIO m', IsSettable w m' a (attr w m' b a)) => attr w m' b a)
     -> SetOnlyAttrib w m (Dynamic t a)
dyn' attr = SetOnlyAttrib' $ \w d -> do setter attr w =<< sample (current d)
                                        es w (updated d)
  where SetOnlyAttrib' es = evt attr

evt :: (NodeGraph t m, IsSettable w (HostFrame t) a (attr w (HostFrame t) b a))
    => attr w (HostFrame t) b a -> SetOnlyAttrib w m (Event t a)
evt attr = SetOnlyAttrib' $ \w e -> onEvent_ e $ setter attr w

-- degenerative combinators following Contravariant.Divisible
divide :: (Monad m, IsSettable w m b attrb, IsSettable w m c attrc)
       => (a -> (b, c)) -> attrb -> attrc -> SetOnlyAttrib w m a
divide f attrb attrc = SetOnlyAttrib' $ \w a -> let (b, c) = f a in do setter attrb w b
                                                                       setter attrc w c

divided :: (Monad m, IsSettable w m b attrb, IsSettable w m c attrc)
        => attrb -> attrc -> SetOnlyAttrib w m (b, c)
divided = divide id

choose :: (Monad m, IsSettable w m b attrb, IsSettable w m c attrc)
       => (a -> Either b c) -> attrb -> attrc -> SetOnlyAttrib w m a
choose f attrb attrc = SetOnlyAttrib' $ \w a -> case f a of
                                                  Left b -> setter attrb w b
                                                  Right c -> setter attrc w c

chosen :: (Monad m, IsSettable w m b attrb, IsSettable w m c attrc)
       => attrb -> attrc -> SetOnlyAttrib w m (Either b c)
chosen = choose id

---- General Attribs ----

class Monad m => HasPosition n m where
  position :: Attrib n m (P2 Double)
  position = attrib (\n -> (^&) <$> gx n <*> gy n)
               (\n p -> let (x, y) = unp2 p in sx n x >> sy n y)
    where Attrib' gx sx _ = positionX
          Attrib' gy sy _ = positionY
  positionX :: Attrib n m Double
  positionX = Attrib' getter' setter' (\n u -> (^._x) <$> updater n (_x %~ u))
    where Attrib' getter _ updater = position
          getter' n = (^._x) <$> getter n
          setter' n x = void $ updater n (_x .~ x)
  positionY :: Attrib n m Double
  positionY = Attrib' getter' setter' (\n u -> (^._y) <$> updater n (_y %~ u))
    where Attrib' getter _ updater = position
          getter' n = (^._y) <$> getter n
          setter' n y = void $ updater n (_y .~ y)

class HasRotation n m where
  rotation :: Attrib n m (Direction V2 Double)

-- Transform is the combination of position and rotation
data Transform = Transform
               { _transformPos :: P2 Double
               , _transformRot :: Direction V2 Double
               }
makeFields ''Transform

transform :: (HasPosition n m, HasRotation n m) => Attrib n m Transform
transform = attrib (\n -> Transform <$> gp n <*> gr n)
              (\n (Transform p r) -> sp n p >> sr n r)
  where Attrib' gp sp _ = position
        Attrib' gr sr _ = rotation
