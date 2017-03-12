{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Concepts largely taken from Graphics.UI.WX.Attributes
module Reflex.Cocos2d.Attributes.Base
    ( Attrib(..)
    , Attrib'
    , ROAttrib(..)
    , ROAttrib'
    , WOAttrib(..)
    , WOAttrib'
    , Prop(..)
    , IsSettable(..)
    , IsGettable(..)
    , mapAttrib
    , get
    , setProps
    , hoistA
    , divide
    , divided
    , choose
    , chosen
    -- * classes and generalizations
    , HasROPositionAttrib(..)
    , HasRWPositionAttrib(..)
    , HasROAngleAttrib(..)
    , HasRWAngleAttrib(..)
    , HasRWTextAttrib(..)
    , positionFrom
    , angleFrom
    , transformFrom
    -- * combinators
    , evt
    , dyn
    , dyn'
    , uDyn
    , uDyn'
    )
  where

import Data.Colour
import Data.Functor.Contravariant
import Diagrams (Point(..), V2(..), P2, (^&), _x, _y, Angle)
import Control.Monad
import Control.Monad.Trans
import Control.Lens hiding (chosen, transform)

import Reflex
import Reflex.Cocos2d.Types

import Graphics.UI.Cocos2d.Common

import Reflex.Extra

infixr 0 :=

data Prop w m = forall a b c. IsSettable w m a (c w m b a) => (c w m b a) := a -- ^ Assign a value to an attribute

class IsSettable w m a attr | attr -> w m a where
  setter :: attr -> w -> a -> m ()

class IsGettable w m a attr | attr -> w m a where
  getter :: attr -> w -> m a

-- | A datatype that supports getter and setter
data Attrib w m b a = Attrib (w -> m b) -- ^ Getter
                             (w -> a -> m ()) -- ^ Setter

type Attrib' w m a = Attrib w m a a

instance IsSettable w m a (Attrib w m b a) where
    setter (Attrib _ s) = s

instance IsGettable w m b (Attrib w m b a) where
    getter (Attrib g _) = g

instance Contravariant (Attrib w m b) where
    contramap f (Attrib g s) = Attrib g s'
      where s' w = s w . f

mapAttrib :: Functor m => (b -> d) -> (c -> a) -> Attrib w m b a -> Attrib w m d c
mapAttrib fg fs (Attrib g s) = Attrib (fmap fg . g) (\w -> s w . fs)

data ROAttrib w m b a = ROAttrib (w -> m a)
type ROAttrib' w m a = forall b. ROAttrib w m b a

instance IsGettable w m a (ROAttrib w m b a) where
    getter (ROAttrib s) = s

data WOAttrib w m b a = WOAttrib (w -> a -> m ())

-- NOTE: the phantom type 'b' is needed because we need the type structure to conform with that of Attrib
-- maybe there is a better way...
type WOAttrib' w m a = forall b. WOAttrib w m b a

instance IsSettable w m a (WOAttrib w m b a) where
    setter (WOAttrib s) = s

instance Contravariant (WOAttrib w m b) where
    contramap f s = WOAttrib $ \w -> setter s w . f

get :: IsGettable n m a attr => n -> attr -> m a
get = flip getter

setProps :: Monad m => w -> [Prop w m] -> m ()
setProps _ [] = return ()
setProps n ((s := a):ps) = setter s n a >> setProps n ps

hoistA :: (forall x. f x -> g x) -> Attrib w f b a -> Attrib w g b a
hoistA trans (Attrib getter setter) = Attrib (trans . getter) (\w -> trans . setter w)

---- combinators ----

-- degenerative combinators following Contravariant.Divisible
divide :: (Monad m, IsSettable w m b attrb, IsSettable w m c attrc)
       => (a -> (b, c)) -> attrb -> attrc -> WOAttrib' w m a
divide f attrb attrc = WOAttrib $ \w a -> let (b, c) = f a in setter attrb w b >> setter attrc w c

divided :: (Monad m, IsSettable w m b attrb, IsSettable w m c attrc)
        => attrb -> attrc -> WOAttrib' w m (b, c)
divided = divide id

choose :: (IsSettable w m b attrb, IsSettable w m c attrc)
       => (a -> Either b c) -> attrb -> attrc -> WOAttrib' w m a
choose f attrb attrc = WOAttrib $ \w a -> case f a of
                                                  Left b -> setter attrb w b
                                                  Right c -> setter attrc w c

chosen :: (IsSettable w m b attrb, IsSettable w m c attrc)
       => attrb -> attrc -> WOAttrib' w m (Either b c)
chosen = choose id

---- General Attribs ----

class Monad m => HasROPositionAttrib w m where
  roPosition :: ROAttrib' w m (P2 Float)

class Monad m => HasRWPositionAttrib w m where
  position :: Attrib' w m (P2 Float)
  position = Attrib (\w -> (^&) <$> gx w <*> gy w)
                     (\w p -> let P (V2 x y) = p in sx w x >> sy w y)
    where Attrib gx sx = positionX
          Attrib gy sy = positionY
  positionX :: Attrib' w m Float
  positionX = Attrib getter' setter'
    where Attrib getter setter = position
          getter' w = (^._x) <$> getter w
          setter' w x = getter w >>= setter w . (_x .~ x)
  positionY :: Attrib' w m Float
  positionY = Attrib getter' setter'
    where Attrib getter setter = position
          getter' w = (^._y) <$> getter w
          setter' w y = getter w >>= setter w . (_y .~ y)

class Monad m => HasROAngleAttrib w m where
  roAngle :: ROAttrib' w m (Angle Float)

class Monad m => HasRWAngleAttrib w m where
  angle :: Attrib' w m (Angle Float)

-- text related general attributes
class Monad m => HasRWTextAttrib w m where
  text :: Attrib' w m String
  horizontalAlign :: Attrib' w m TextHAlignment
  verticalAlign :: Attrib' w m TextVAlignment
  textColor :: Attrib' w m (AlphaColour Float)
  outline :: WOAttrib' w m (Maybe Outline)
  shadow :: WOAttrib' w m (Maybe Shadow)
  glow :: WOAttrib' w m (Maybe Glow)

-- take position from a given widget that has position
positionFrom :: (HasROPositionAttrib a m, HasRWPositionAttrib w m) => WOAttrib' w m a
positionFrom = WOAttrib $ \w a -> get a roPosition >>= setter position w

angleFrom :: (HasROAngleAttrib a m, HasRWAngleAttrib w m) => WOAttrib' w m a
angleFrom = WOAttrib $ \w a -> get a roAngle >>= setter angle w

transformFrom :: ( HasROAngleAttrib a m, HasROPositionAttrib a m
                 , HasRWAngleAttrib w m, HasRWPositionAttrib w m )
              => WOAttrib' w m a
transformFrom = WOAttrib $ \w a -> setter positionFrom w a >> setter angleFrom w a


-- | Higher-order combinators
evt :: ( PerformEvent t m
       , IsSettable w (Performable m) a (attr w (Performable m) b a) )
    => attr w (Performable m) b a -> WOAttrib' w m (Event t a)
evt attr = WOAttrib $ \w e -> performEvent_ . ffor e $ setter attr w

-- | Transforms a IsSettable attribute to a WOAttribute. This uses lazy read on the incoming Dynamic
dyn :: ( PerformEvent t m, PostBuild t m
       , IsSettable w (Performable m) a (attr w (Performable m) b a) )
    => attr w (Performable m) b a -> WOAttrib' w m (Dynamic t a)
dyn attr = WOAttrib $ \w -> setter evtattr w <=< postponeCurrent
  where evtattr = evt attr


uDyn :: ( PerformEvent t m, PostBuild t m
        , IsSettable w (Performable m) a (attr w (Performable m) b a)
        , Eq a )
     => attr w (Performable m) b a -> WOAttrib' w m (UniqDynamic t a)
uDyn = (fromUniqDynamic >$<) . dyn

-- | Similar to `dyn`, but applies strict read
-- XXX: nasty constraints to allow 'c' to be instantiated as different types within the function
dyn' :: forall attr w t m b a.
        ( PerformEvent t m
        , MonadIO m, MonadSample t m
        , MonadIO (Performable m)
        , IsSettable w (Performable m) a (attr w (Performable m) b a)
        , IsSettable w m a (attr w m b a) )
     => (forall m'. (MonadIO m', IsSettable w m' a (attr w m' b a)) => attr w m' b a)
     -> WOAttrib' w m (Dynamic t a)
dyn' attr = WOAttrib $ \w d -> do
              setter attr w =<< sample (current d)
              setter (evt (attr :: attr w (Performable m) b a)) w (updated d)

uDyn' :: ( PerformEvent t m
         , MonadIO m, MonadSample t m
         , MonadIO (Performable m)
         , IsSettable w (Performable m) a (attr w (Performable m) b a)
         , IsSettable w m a (attr w m b a)
         , Eq a )
      => (forall m'. (MonadIO m', IsSettable w m' a (attr w m' b a)) => attr w m' b a)
      -> WOAttrib' w m (UniqDynamic t a)
uDyn' attr = fromUniqDynamic >$< dyn' attr
