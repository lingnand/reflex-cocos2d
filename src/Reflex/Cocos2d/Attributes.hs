{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Concepts largely taken from Graphics.UI.WX.Attributes
module Reflex.Cocos2d.Attributes
    ( Attrib'(..)
    , Attrib
    , SetOnlyAttrib'(..)
    , SetOnlyAttrib
    , Prop(..)
    , get
    , setProps
    , hoistA
    , dyn
    , dyn'
    , uDyn
    , uDyn'
    , evt
    , divide
    , divided
    , choose
    , chosen
    -- attrs --
    , HasPosition(..)
    , HasAngle(..)
    , HasText(..)
    , positionFrom
    , angleFrom
    , transformFrom
    )
  where

import Data.Colour
import Data.Functor.Contravariant
import Diagrams (Point(..), V2(..), P2, (^&), _x, _y, Angle)
import Control.Lens hiding (chosen, transform)
import Control.Monad.Trans
import Reflex.Host.Class
import Reflex
import Reflex.Cocos2d.Class
import Reflex.Cocos2d.Types

import Graphics.UI.Cocos2d.Common

infixr 0 :=

data Prop w m = forall a b c. IsSettable w m a (c w m b a) => (c w m b a) := a -- ^ Assign a value to an attribute

class IsSettable w m a attr | attr -> w m a where
  setter :: attr -> w -> a -> m ()

class IsGettable w m a attr | attr -> w m a where
  getter :: attr -> w -> m a

-- | A datatype that supports getter and setter
data Attrib' w m b a = Attrib' (w -> m b) -- ^ Getter
                               (w -> a -> m ()) -- ^ Setter

type Attrib w m a = Attrib' w m a a

instance IsSettable w m a (Attrib' w m b a) where
    setter (Attrib' _ s) = s

instance IsGettable w m b (Attrib' w m b a) where
    getter (Attrib' g _) = g

instance Contravariant (Attrib' w m b) where
    contramap f (Attrib' g s) = Attrib' g s'
      where s' w = s w . f

data SetOnlyAttrib' w m b a = SetOnlyAttrib' (w -> a -> m ())

-- NOTE: the phantom type 'b' is needed because we need the type structure to conform with that of Attrib'
-- maybe there is a better way...
type SetOnlyAttrib w m a = forall b. SetOnlyAttrib' w m b a

instance IsSettable w m a (SetOnlyAttrib' w m b a) where
    setter (SetOnlyAttrib' s) = s

instance Contravariant (SetOnlyAttrib' w m b) where
    contramap f s = SetOnlyAttrib' $ \w -> setter s w . f

get :: IsGettable n m a attr => n -> attr -> m a
get = flip getter

setProps :: Monad m => w -> [Prop w m] -> m ()
setProps _ [] = return ()
setProps n ((s := a):ps) = setter s n a >> setProps n ps

hoistA :: (forall x. f x -> g x)  -> Attrib' w f b a -> Attrib' w g b a
hoistA trans (Attrib' getter setter) = Attrib' (trans . getter) (\w -> trans . setter w)

---- combinators ----

-- | Transforms a IsSettable attribute to a SetOnlyAttribute. This uses lazy read on the incoming Dynamic
dyn :: (NodeGraph t m, IsSettable w (HostFrame t) a (attr w (HostFrame t) b a))
    => attr w (HostFrame t) b a -> SetOnlyAttrib w m (Dynamic t a)
dyn attr = SetOnlyAttrib' $ \w d -> do
            e <- view postBuildEvent
            runEvent_ $ (setter attr w =<< sample (current d)) <$ e
            let SetOnlyAttrib' es = evt attr
            es w (updated d)

-- | Similar to `dyn`, but applies strict read
-- XXX: nasty constraints to allow 'c' to be instantiated as different types within the function
dyn' :: ( NodeGraph t m
        , IsSettable w (HostFrame t) a (attr w (HostFrame t) b a)
        , IsSettable w m a (attr w m b a) )
     => (forall m'. (MonadIO m', IsSettable w m' a (attr w m' b a)) => attr w m' b a)
     -> SetOnlyAttrib w m (Dynamic t a)
dyn' attr = SetOnlyAttrib' $ \w d -> do
              setter attr w =<< sample (current d)
              let SetOnlyAttrib' es = evt attr
              es w (updated d)

uDyn :: (NodeGraph t m, IsSettable w (HostFrame t) a (attr w (HostFrame t) b a), Eq a)
     => attr w (HostFrame t) b a -> SetOnlyAttrib w m (UniqDynamic t a)
uDyn = (fromUniqDynamic >$<) . dyn

uDyn' :: ( NodeGraph t m
         , IsSettable w (HostFrame t) a (attr w (HostFrame t) b a)
         , IsSettable w m a (attr w m b a)
         , Eq a )
      => (forall m'. (MonadIO m', IsSettable w m' a (attr w m' b a)) => attr w m' b a)
      -> SetOnlyAttrib w m (UniqDynamic t a)
uDyn' attr = fromUniqDynamic >$< dyn' attr

evt :: (NodeGraph t m, IsSettable w (HostFrame t) a (attr w (HostFrame t) b a))
    => attr w (HostFrame t) b a -> SetOnlyAttrib w m (Event t a)
evt attr = SetOnlyAttrib' $ \w e -> onEvent_ e $ setter attr w

-- degenerative combinators following Contravariant.Divisible
divide :: (Monad m, IsSettable w m b attrb, IsSettable w m c attrc)
       => (a -> (b, c)) -> attrb -> attrc -> SetOnlyAttrib w m a
divide f attrb attrc = SetOnlyAttrib' $ \w a -> let (b, c) = f a in setter attrb w b >> setter attrc w c

divided :: (Monad m, IsSettable w m b attrb, IsSettable w m c attrc)
        => attrb -> attrc -> SetOnlyAttrib w m (b, c)
divided = divide id

choose :: (IsSettable w m b attrb, IsSettable w m c attrc)
       => (a -> Either b c) -> attrb -> attrc -> SetOnlyAttrib w m a
choose f attrb attrc = SetOnlyAttrib' $ \w a -> case f a of
                                                  Left b -> setter attrb w b
                                                  Right c -> setter attrc w c

chosen :: (IsSettable w m b attrb, IsSettable w m c attrc)
       => attrb -> attrc -> SetOnlyAttrib w m (Either b c)
chosen = choose id

---- General Attribs ----

class Monad m => HasPosition w m where
  position :: Attrib w m (P2 Float)
  position = Attrib' (\w -> (^&) <$> gx w <*> gy w)
                     (\w p -> let P (V2 x y) = p in sx w x >> sy w y)
    where Attrib' gx sx = positionX
          Attrib' gy sy = positionY
  positionX :: Attrib w m Float
  positionX = Attrib' getter' setter'
    where Attrib' getter setter = position
          getter' w = (^._x) <$> getter w
          setter' w x = getter w >>= setter w . (_x .~ x)
  positionY :: Attrib w m Float
  positionY = Attrib' getter' setter'
    where Attrib' getter setter = position
          getter' w = (^._y) <$> getter w
          setter' w y = getter w >>= setter w . (_y .~ y)

class Monad m => HasAngle w m where
  angle :: Attrib w m (Angle Float)

-- text related general attributes
class Monad m => HasText w m where
  text :: Attrib w m String
  horizontalAlign :: Attrib w m TextHAlignment
  verticalAlign :: Attrib w m TextVAlignment
  textColor :: Attrib w m (AlphaColour Float)
  outline :: SetOnlyAttrib w m (Maybe Outline)
  shadow :: SetOnlyAttrib w m (Maybe Shadow)
  glow :: SetOnlyAttrib w m (Maybe Glow)

-- take position from a given widget that has position
positionFrom :: (HasPosition a m, HasPosition w m) => SetOnlyAttrib w m a
positionFrom = SetOnlyAttrib' $ \w a -> get a position >>= setter position w

angleFrom :: (HasAngle a m, HasAngle w m) => SetOnlyAttrib w m a
angleFrom = SetOnlyAttrib' $ \w a -> get a angle >>= setter angle w

transformFrom :: (HasAngle a m, HasPosition a m, HasAngle w m, HasPosition w m)
              => SetOnlyAttrib w m a
transformFrom = SetOnlyAttrib' $ \w a -> fp w a >> fa w a
  where SetOnlyAttrib' fp = positionFrom
        SetOnlyAttrib' fa = angleFrom
