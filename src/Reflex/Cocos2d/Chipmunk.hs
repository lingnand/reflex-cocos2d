{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
module Reflex.Cocos2d.Chipmunk
    (
      Maskable(..)

    , Space

    , Shape
    , shapeBody
    , category

    , Body

    , SpaceStep

    , space

    , iterations
    , gravity
    , damping
    , collisionSlop
    , collisionBias

    , CollisionEvents
    , getCollisionEvents

    , body

    , mass
    , moment
    , velocity
    , maxVelocity
    , force
    , impulse
    , maxAngVel
    , torque
    , active

    , localToWorld
    , worldToLocal
      -- re-export
    , H.HasShapeDefinition(..)
    , ShapeDefinition
    )
  where

import Data.Word
import Data.Bits
import Data.Default
import qualified Data.StateVar as S
import Data.Dependent.Sum ((==>))
import Data.StateVar
import Diagrams (rad, (@@), V2, P2)
import qualified Physics.Hipmunk as H
import Control.Lens
import Control.Monad.Ref
import Control.Monad.Trans
import Control.Monad.Reader

import Reflex
import Reflex.Host.Class
import Reflex.Cocos2d.Attributes
import Reflex.Cocos2d.Class

class Maskable a where
    toMask :: a -> Word64
    fromMask :: Word64 -> a
    default toMask :: Enum a => a -> Word64
    toMask a | i >= 64 = error $ show i ++ ": Exceeded maximum allowed number of bits"
             | otherwise = bit i
      where i = fromEnum a
    default fromMask :: Enum a => Word64 -> a
    -- NOTE: this assumes that the incoming Word64 has *only* one bit set
    fromMask = toEnum . countTrailingZeros

-- each Space can only accept a given collision type
newtype Space a = SPWrap H.Space deriving (Eq, Ord)

-- data Contact = Contact
--     { _contactShapeA :: H.Shape
--     , _contactShapeB :: H.Shape
--     , _contactNormal :: V2 Double -- TODO: do we need this?
--     , _contactPoints :: [V2 Double] -- TODO: do we need this?
--     }

newtype ShapeDefinition a = SDWrap { unwrapSD :: H.ShapeDefinition Float } deriving (Eq, Ord, Show)

instance H.HasShapeDefinition (ShapeDefinition a) Float where
    shapeDefinition = lens unwrapSD (const SDWrap) . H.shapeDefinition

shapeDefinitionRealToFrac :: (Real a, Fractional b) => H.ShapeDefinition a -> H.ShapeDefinition b
shapeDefinitionRealToFrac (H.ShapeDefinition t os mass ctm cm) =
    H.ShapeDefinition t (realToFrac <$> os) (realToFrac mass) ctm cm

instance Default (ShapeDefinition a) where
    def = SDWrap def

newtype Shape a = SWrap { unwrapS :: H.Shape } deriving (Eq, Ord)

-- force Float ShapeDefinition
instance H.HasShapeDefinition (Shape a) Float where
    shapeDefinition = lens unwrapS (const SWrap) . H.shapeDefinition
                    . lens shapeDefinitionRealToFrac (const shapeDefinitionRealToFrac)

shapeBody :: Getter (Shape a) (Body a)
shapeBody = to unwrapS . H.shapeBody . to BWrap

category :: (Maskable a, H.HasShapeDefinition (sd a) Float) => Lens' (sd a) a
category = H.categoryMask . lens fromMask (const toMask)

newtype Body a = BWrap H.Body deriving (Eq, Ord)

class BodyPtr a where
    toBody :: a -> H.Body

class SpacePtr a where
    toSpace :: a -> H.Space

instance BodyPtr (Body a) where
    toBody (BWrap b) = b

instance BodyPtr b => BodyPtr (a, b) where
    toBody (_, b) = toBody b

instance SpacePtr (Space a) where
    toSpace (SPWrap s) = s

instance SpacePtr s => SpacePtr (s, a) where
    toSpace (s, _) = toSpace s

type SpaceStep = Time

space :: NodeGraph t m => Event t Time -> [Prop (Space a) m] -> m (Space a, Event t SpaceStep)
space ts props = do
    sp <- liftIO H.newSpace
    let wrapped = SPWrap sp
    setProps wrapped props
    fmap (wrapped,) . onEvent ts $ \dt -> liftIO $ do
      H.step sp $ realToFrac dt
      return dt

data CollisionEvents t a = CollisionEvents
    { _collisionBegan :: Event t (Shape a, Shape a)
    , _collisionEnded :: Event t (Shape a, Shape a)
    }

-- TODO: lens

getCollisionEvents :: NodeGraph t m => Space a -> m (CollisionEvents t a)
getCollisionEvents (SPWrap sp) = do
    (eBegin, trBegin) <- newEventWithTriggerRef
    (eSeparate, trSeparate) <- newEventWithTriggerRef
    run <- view runWithActions
    liftIO $ H.setDefaultCollisionHandler sp H.Handler
      { H.beginHandler = Just $ do
          (sa, sb) <- H.shapes
          -- check for category
          if sa^.H.collisionMask .&. sb^.H.categoryMask == 0
            || sb^.H.collisionMask .&. sa^.H.categoryMask == 0
            then return False
            else do
              H.postStep sa $ do
                liftIO $ readRef trBegin
                  >>= mapM_ (\tr -> run ([tr ==> (SWrap sa, SWrap sb)], return ()))
              return True
      , H.preSolveHandler = Nothing
      , H.postSolveHandler = Nothing
      , H.separateHandler = Just $ do
          (sa, sb) <- H.shapes
          H.postStep sa $ do
            liftIO $ readRef trSeparate
              >>= mapM_ (\tr -> run ([tr ==> (SWrap sa, SWrap sb)], return ()))
      }
    return $ CollisionEvents eBegin eSeparate

-- instance below to enable NodeGraph lifting
body :: MonadIO m
     => Space a
     -> [ShapeDefinition a]
     -> [Prop (Space a, Body a) m]
     -> m (Body a)
body wsp@(SPWrap sp) shapes props = do
    bd <- liftIO $ do
      -- XXX: use 1 1 to bypass checks to create a body
      b <- H.newBody 1 1
      -- add shape
      let rawSs = unwrapSD <$> shapes
          moment sh = H.momentForShape
            (realToFrac $ sh^.H.shapeMass)
            (sh^.H.shapeType)
            (realToFrac <$> sh^.H.shapeOffset)
          totalMoment = sum $ moment <$> rawSs
          totalMass = realToFrac $ sumOf (folded.H.shapeMass) rawSs
      forM_ rawSs $ H.spaceAdd sp <=< H.newShape b . shapeDefinitionRealToFrac
      H.mass b S.$= totalMass
      H.moment b S.$= totalMoment
      return b
    let wrapped = BWrap bd
    setProps (wsp, wrapped) props
    return wrapped

liftStateVar :: MonadIO m => (b -> StateVar a) -> Attrib b m a
liftStateVar f = Attrib' (S.get . f) ((S.$=) . f)
{-# INLINE liftStateVar #-}

liftStateVarToFloat :: MonadIO m => (b -> StateVar Double) -> Attrib b m Float
liftStateVarToFloat = liftStateVar . (S.mapStateVar realToFrac realToFrac .)
{-# INLINE liftStateVarToFloat #-}

liftStateVarToMappedFloat :: (MonadIO m, Functor f) => (b -> StateVar (f Double)) -> Attrib b m (f Float)
liftStateVarToMappedFloat = liftStateVar . (S.mapStateVar (fmap realToFrac) (fmap realToFrac) .)
{-# INLINE liftStateVarToMappedFloat #-}

-- attributes for space

iterations :: (MonadIO m, SpacePtr s) => Attrib s m Int
iterations = liftStateVar $ S.mapStateVar fromIntegral fromIntegral . H.iterations . toSpace

gravity :: (MonadIO m, SpacePtr s) => Attrib s m (V2 Float)
gravity = liftStateVarToMappedFloat $ H.gravity . toSpace

damping :: (MonadIO m, SpacePtr s) => Attrib s m Float
damping = liftStateVarToFloat $ H.damping . toSpace

collisionSlop :: (MonadIO m, SpacePtr s) => Attrib s m Float
collisionSlop = liftStateVarToFloat $ H.collisionSlop . toSpace

collisionBias :: (MonadIO m, SpacePtr s) => Attrib s m Float
collisionBias = liftStateVarToFloat $ H.collisionBias . toSpace
-- attributes for bodies

-- we use Float for everything here...
mass :: (MonadIO m, BodyPtr b) => Attrib b m Float
mass = liftStateVarToFloat $ S.mapStateVar realToFrac realToFrac . H.mass . toBody

moment :: (MonadIO m, BodyPtr b) => Attrib b m Float
moment = liftStateVarToFloat (H.moment . toBody)

instance (MonadIO m, BodyPtr b) => HasAngle b m where
    angle = liftStateVar $ S.mapStateVar (realToFrac . (^.rad)) ((@@ rad) . realToFrac) . H.angle . toBody

instance (MonadIO m, BodyPtr b) => HasPosition b m where
    position = liftStateVarToMappedFloat $ H.position . toBody

velocity :: (MonadIO m, BodyPtr b) => Attrib b m (V2 Float)
velocity = liftStateVarToMappedFloat $ H.velocity . toBody

maxVelocity :: (MonadIO m, BodyPtr b) => Attrib b m Float
maxVelocity = liftStateVarToFloat $ H.maxVelocity . toBody

-- apply a list of forces to the current body
force :: (MonadIO m, BodyPtr b) => SetOnlyAttrib b m [(V2 Float, P2 Float)]
force = SetOnlyAttrib' appForces
  where appForces _ [] = return ()
        appForces bp ((f, offset):fs) = liftIO $ do
          let b = toBody bp
          H.applyOnlyForce b (realToFrac <$> f) (realToFrac <$> offset)
          forM_ fs $ \(f, offset) -> do
            H.applyForce b (realToFrac <$> f) (realToFrac <$> offset)

impulse :: (MonadIO m, BodyPtr b) => SetOnlyAttrib b m (V2 Float, P2 Float)
impulse = SetOnlyAttrib' $ \bp (imp, offset) -> liftIO $ do
            let b = toBody bp
            H.applyImpulse b (realToFrac <$> imp) (realToFrac <$> offset)

maxAngVel :: (MonadIO m, BodyPtr b) => Attrib b m Float
maxAngVel = liftStateVarToFloat $ H.maxAngVel . toBody

torque :: (MonadIO m, BodyPtr b) => Attrib b m Float
torque = liftStateVarToFloat $ H.torque . toBody

-- whether the body is added to the space
active :: (MonadIO m, BodyPtr a, SpacePtr a) => Attrib a m Bool
active = Attrib' getter setter
  where setter p act =
          let b = toBody p
              sp = toSpace p
          in case act of
            True -> liftIO $ H.inSpace b >>= \case
                True -> return ()
                _ -> H.spaceAdd sp b
            False -> liftIO $ H.inSpace b >>= \case
                True -> H.spaceRemove sp b
                _ -> return ()
        getter = liftIO . H.inSpace . toBody

localToWorld :: MonadIO m => Body a -> P2 Float -> m (P2 Float)
localToWorld (BWrap b) p = liftIO $ fmap realToFrac <$> H.localToWorld b (realToFrac <$> p)

worldToLocal :: MonadIO m => Body a -> P2 Float -> m (P2 Float)
worldToLocal (BWrap b) p = liftIO $ fmap realToFrac <$> H.worldToLocal b (realToFrac <$> p)
