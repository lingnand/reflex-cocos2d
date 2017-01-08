{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
    , ShapeAttributes
    , shapeBody
    -- these are actually modifiable attributes...
    -- , shapeGeometry
    -- , shapeOffset
    -- , shapeMass
    ----------------------
    , shapeCategory
    , shapeCategoryMask
    , shapeCollisionMask

    , Body

    , SpaceStep

    , space

    , iterations
    , gravity
    , damping
    , collisionSlop
    , collisionBias

    , CollisionEvents(CollisionEvents)
    , collisionBegan
    , collisionEnded
    , getCollisionEvents

    , fanCollisionsByBody

    , body

    , velocity
    , maxVelocity
    , force
    , impulse
    , maxAngVel
    , torque
    , active

    , shape

    , shapeAttributes
    , mass
    , offset
    , geometry
    , categoryMask
    , category
    , collisionMask
    , group
    , elasticity
    , friction
    , surfaceVel

    , localToWorld
    , worldToLocal
      -- re-export
    , H.Geometry(..)
    )
  where

import Data.Word
import Data.Bits
import Data.Functor.Misc
import qualified Data.Map as M
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

newtype ShapeAttributes a = SAWrap { unwrapSA :: H.ShapeAttributes Float } deriving (Eq, Ord)

toSA :: Real a => H.ShapeAttributes a -> ShapeAttributes x
toSA (H.ShapeAttributes bd t os mass ctm cm) = SAWrap $
    H.ShapeAttributes bd (realToFrac <$> t) (realToFrac <$> os) (realToFrac mass) ctm cm

newtype Shape a = SWrap { unwrapS :: H.Shape } deriving (Eq, Ord)

-- override Entity instance to have automatic mass/moment recomputation
instance H.Entity (Shape a) where
    spaceAdd sp (SWrap s) = do
      H.spaceAdd sp s
      bd <- view H.shapeBody <$> S.get (H.shapeAttributes s)
      H.recomputeTotalMassAndMoment sp bd
    spaceRemove sp (SWrap s) = do
      H.spaceRemove sp s
      bd <- view H.shapeBody <$> S.get (H.shapeAttributes s)
      H.recomputeTotalMassAndMoment sp bd
    entityPtr = H.entityPtr . unwrapS
    inSpace   = H.inSpace . unwrapS


wrappedSA :: Lens' (ShapeAttributes a) (H.ShapeAttributes Float)
wrappedSA = lens unwrapSA (const SAWrap)

shapeBody :: Getter (ShapeAttributes a) (Body a)
shapeBody = wrappedSA . H.shapeBody . to BWrap

shapeCategoryMask :: Lens' (ShapeAttributes a) Word64
shapeCategoryMask = wrappedSA . H.shapeCategoryMask

shapeCategory :: Maskable a => Lens' (ShapeAttributes a) a
shapeCategory = shapeCategoryMask . lens fromMask (const toMask)

shapeCollisionMask :: Lens' (ShapeAttributes a) Word64
shapeCollisionMask = wrappedSA . H.shapeCollisionMask


newtype Body a = BWrap H.Body deriving (Eq, Ord, H.Entity)

class BodyPtr a e | e -> a where
    toBody :: e -> H.Body

class SpacePtr a e | e -> a where
    toSpace :: e -> H.Space

class ShapePtr a e | e -> a where
    toShape :: e -> H.Shape

instance BodyPtr a (Body a) where
    toBody (BWrap b) = b

instance BodyPtr a b => BodyPtr a (x, b) where
    toBody (_, b) = toBody b

instance ShapePtr a (Shape a) where
    toShape (SWrap s) = s

instance ShapePtr a b => ShapePtr a (x, b) where
    toShape (_, s) = toShape s

instance H.Entity e => H.Entity (x, e) where
    spaceAdd sp (_, e) = H.spaceAdd sp e
    spaceRemove sp (_, e) = H.spaceRemove sp e
    entityPtr (_, e) = H.entityPtr e
    inSpace (_, e) = H.inSpace e

instance SpacePtr a (Space a) where
    toSpace (SPWrap s) = s

instance SpacePtr a s => SpacePtr a (s, x) where
    toSpace (s, _) = toSpace s

type SpaceStep = Time

-- | create and run a space at frame rate
space :: NodeBuilder t m
      => [Prop (Space a) m] -> m (Space a, Event t SpaceStep)
space props = do
    ts <- view frameTicks
    sp <- liftIO H.newSpace
    addFinalizer  . liftIO $ H.freeSpace sp
    let wrapped = SPWrap sp
    setProps wrapped props
    fmap (wrapped,) . forEvent ts $ \dt -> liftIO $ do
      H.step sp $ realToFrac dt
      return dt

data CollisionEvents t a = CollisionEvents
    { _collisionBegan :: Event t (ShapeAttributes a, ShapeAttributes a)
    , _collisionEnded :: Event t (ShapeAttributes a, ShapeAttributes a)
    }

collisionBegan :: Lens' (CollisionEvents t a) (Event t (ShapeAttributes a, ShapeAttributes a))
collisionBegan f (CollisionEvents began ended)
  = fmap
      (\ began' -> CollisionEvents began' ended) (f began)
{-# INLINE collisionBegan #-}
collisionEnded :: Lens' (CollisionEvents t a) (Event t (ShapeAttributes a, ShapeAttributes a))
collisionEnded f (CollisionEvents began ended)
  = fmap
      (\ ended' -> CollisionEvents began ended') (f ended)
{-# INLINE collisionEnded #-}

getCollisionEvents :: NodeBuilder t m => Space a -> m (CollisionEvents t a)
getCollisionEvents (SPWrap sp) = do
    (eBegin, trBegin) <- newEventWithTriggerRef
    (eSeparate, trSeparate) <- newEventWithTriggerRef
    run <- view runWithActions
    liftIO $ H.setDefaultCollisionHandler sp H.Handler
      { H.beginHandler = Just $ do
          H.shapes >>= \case
            Just (sa, sb) -> do
              -- check for category
              saa <- S.get (H.shapeAttributes sa)
              sab <- S.get (H.shapeAttributes sb)
              if saa^.H.shapeCollisionMask .&. sab^.H.shapeCategoryMask == 0
                || sab^.H.shapeCollisionMask .&. saa^.H.shapeCategoryMask == 0
                then return False
                else do
                  H.postStep sa $ do
                    liftIO $ readRef trBegin
                      >>= mapM_ (\tr -> run ([tr ==> (toSA saa, toSA sab)], return ()))
                  return True
            _ -> return False
      , H.preSolveHandler = Nothing
      , H.postSolveHandler = Nothing
      , H.separateHandler = Just $ do
          -- XXX: a bit of optimization: don't check shapes if we don't even have the trigger
          liftIO (readRef trSeparate) >>= \case
            Just tr -> do
              H.shapes >>= \case
                Just (sa, sb) -> do
                  saa <- S.get (H.shapeAttributes sa)
                  sab <- S.get (H.shapeAttributes sb)
                  H.postStep sa $ do
                    liftIO $ run ([tr ==> (toSA saa, toSA sab)], return ())
                _ -> return ()
            _ -> return ()
      }
    return $ CollisionEvents eBegin eSeparate

fanCollisionsByBody :: Reflex t => Event t (ShapeAttributes a, ShapeAttributes a) -> EventSelector t (Const2 (Body a) (ShapeAttributes a))
fanCollisionsByBody = fanMap . fmap trans
  where trans (sa, sb) = M.fromList
          [ (s1^.shapeBody, s2)
          | (s1, s2) <- [(sa, sb), (sb, sa)]
          ]

safeAdd :: (SpacePtr a sp, H.Entity e, MonadIO m) => sp -> e -> m ()
safeAdd sp e = liftIO $ H.inSpace e >>= \case
    True -> return ()
    _ -> H.spaceAdd (toSpace sp) e

safeRemove :: (SpacePtr a sp, H.Entity e, MonadIO m) => sp -> e -> m ()
safeRemove sp e = liftIO $ H.inSpace e >>= \case
    True -> H.spaceRemove (toSpace sp) e
    _ -> return ()

-- | NOTE: to add the body to the space, you have to pass in the active := true prop
body :: NodeBuilder t m
     => Space a
     -> [Prop (Space a, Body a) m]
     -> m (Body a)
body wsp props = do
    bd <- liftIO $ H.newBody (1/0) (1/0)
    -- add a finalizer so that if the body is still in the space, remove it
    let wrapped = BWrap bd
    addFinalizer $ safeRemove wsp wrapped
    setProps (wsp, wrapped) props
    return wrapped

-- | NOTE: to add the shape to the space, you have to pass in the active := true prop
shape :: NodeBuilder t m
      => Space a
      -> Body a
      -> H.Geometry Float
      -> [Prop (Space a, Shape a) m]
      -> m (Shape a)
shape wsp (BWrap b) geo props = do
    sh <- liftIO . H.newShape $ H.ShapeAttributes
            b                    -- body
            (realToFrac <$> geo) -- geo
            0                    -- offset
            (1/0)                -- mass
            0                    -- categoryMask
            maxBound             -- collisionMask
    let wrapped = SWrap sh
    -- XXX: removing shapes one by one with recompute is potentially
    -- expensive...?
    addFinalizer $ safeRemove wsp wrapped
    setProps (wsp, wrapped) props
    return wrapped


liftStateVar :: MonadIO m => (b -> StateVar a) -> Attrib' b m a
liftStateVar f = Attrib (S.get . f) ((S.$=) . f)
{-# INLINE liftStateVar #-}

liftStateVarToFloat :: MonadIO m => (b -> StateVar Double) -> Attrib' b m Float
liftStateVarToFloat = liftStateVar . (S.mapStateVar realToFrac realToFrac .)
{-# INLINE liftStateVarToFloat #-}

liftStateVarToMappedFloat :: (MonadIO m, Functor f) => (b -> StateVar (f Double)) -> Attrib' b m (f Float)
liftStateVarToMappedFloat = liftStateVar . (S.mapStateVar (fmap realToFrac) (fmap realToFrac) .)
{-# INLINE liftStateVarToMappedFloat #-}

-- attributes for space

iterations :: (MonadIO m, SpacePtr a s) => Attrib' s m Int
iterations = liftStateVar $ S.mapStateVar fromIntegral fromIntegral . H.iterations . toSpace

gravity :: (MonadIO m, SpacePtr a s) => Attrib' s m (V2 Float)
gravity = liftStateVarToMappedFloat $ H.gravity . toSpace

damping :: (MonadIO m, SpacePtr a s) => Attrib' s m Float
damping = liftStateVarToFloat $ H.damping . toSpace

collisionSlop :: (MonadIO m, SpacePtr a s) => Attrib' s m Float
collisionSlop = liftStateVarToFloat $ H.collisionSlop . toSpace

collisionBias :: (MonadIO m, SpacePtr a s) => Attrib' s m Float
collisionBias = liftStateVarToFloat $ H.collisionBias . toSpace

-- attributes for bodies

instance MonadIO m => HasRWAngleAttrib (Body a) m where
    angle = liftStateVar $ S.mapStateVar (realToFrac . (^.rad)) ((@@ rad) . realToFrac) . H.angle . toBody

instance MonadIO m => HasRWPositionAttrib (Body a) m where
    position = liftStateVarToMappedFloat $ H.position . toBody

instance {-# OVERLAPPING #-} MonadIO m => HasRWAngleAttrib (x, Body a) m where
    angle = Attrib (getter . snd) (setter . snd)
      where Attrib getter setter = angle

instance {-# OVERLAPPING #-} MonadIO m => HasRWPositionAttrib (x, Body a) m where
    position = Attrib (getter . snd) (setter . snd)
      where Attrib getter setter = position

velocity :: (MonadIO m, BodyPtr a b) => Attrib' b m (V2 Float)
velocity = liftStateVarToMappedFloat $ H.velocity . toBody

maxVelocity :: (MonadIO m, BodyPtr a b) => Attrib' b m Float
maxVelocity = liftStateVarToFloat $ H.maxVelocity . toBody

-- apply a list of forces to the current body
force :: (MonadIO m, BodyPtr a b) => WOAttrib' b m [(V2 Float, P2 Float)]
force = WOAttrib appForces
  where appForces _ [] = return ()
        appForces bp ((f, offset):fs) = liftIO $ do
          let b = toBody bp
          H.applyOnlyForce b (realToFrac <$> f) (realToFrac <$> offset)
          forM_ fs $ \(f, offset) -> do
            H.applyForce b (realToFrac <$> f) (realToFrac <$> offset)

impulse :: (MonadIO m, BodyPtr a b) => WOAttrib' b m (V2 Float, P2 Float)
impulse = WOAttrib $ \bp (imp, offset) -> liftIO $ do
            let b = toBody bp
            H.applyImpulse b (realToFrac <$> imp) (realToFrac <$> offset)

maxAngVel :: (MonadIO m, BodyPtr a b) => Attrib' b m Float
maxAngVel = liftStateVarToFloat $ H.maxAngVel . toBody

torque :: (MonadIO m, BodyPtr a b) => Attrib' b m Float
torque = liftStateVarToFloat $ H.torque . toBody

-- whether the body is added to the space
active :: (MonadIO m, H.Entity e, SpacePtr a e) => Attrib' e m Bool
active = Attrib getter setter
  where setter p act = case act of
            True -> safeAdd p p
            False -> safeRemove p p
        getter = liftIO . H.inSpace

-- attributes for shapes

shapeAttributes :: (MonadIO m, ShapePtr a s) => ROAttrib' s m (ShapeAttributes a)
shapeAttributes = ROAttrib $ fmap toSA . S.get . H.shapeAttributes . toShape

mass :: (MonadIO m, ShapePtr a e, SpacePtr a e) => Attrib' e m Float
mass = Attrib getter setter
  where getter = fmap realToFrac . S.get . H.mass . toShape
        setter w v = liftIO $ do
          let sh = toShape w
          H.mass sh S.$= realToFrac v
          act <- H.inSpace sh
          when act $ do
            bd <- view H.shapeBody <$> S.get (H.shapeAttributes sh)
            H.recomputeTotalMassAndMoment (toSpace w) bd

offset :: (MonadIO m, ShapePtr a e, SpacePtr a e) => Attrib' e m (P2 Float)
offset = Attrib getter setter
  where getter = fmap (realToFrac <$>) . S.get . H.offset . toShape
        setter w v = liftIO $ do
          let sh = toShape w
          H.offset sh S.$= realToFrac <$> v
          act <- H.inSpace sh
          when act $ do
            bd <- view H.shapeBody <$> S.get (H.shapeAttributes sh)
            H.recomputeTotalMassAndMoment (toSpace w) bd

geometry :: (MonadIO m, ShapePtr a e, SpacePtr a e) => Attrib' e m (H.Geometry Float)
geometry = Attrib getter setter
  where getter = fmap (realToFrac <$>) . S.get . H.geometry . toShape
        setter w v = liftIO $ do
          let sh = toShape w
          H.geometry sh S.$= realToFrac <$> v
          act <- H.inSpace sh
          when act $ do
            bd <- view H.shapeBody <$> S.get (H.shapeAttributes sh)
            H.recomputeTotalMassAndMoment (toSpace w) bd

categoryMask :: (MonadIO m, ShapePtr a s) => Attrib' s m Word64
categoryMask = liftStateVar $ H.categoryMask . toShape

category :: (Maskable a, MonadIO m, ShapePtr a s) => Attrib' s m a
category = mapAttrib fromMask toMask categoryMask

collisionMask :: (MonadIO m, ShapePtr a s) => Attrib' s m Word64
collisionMask = liftStateVar $ H.collisionMask . toShape

group :: (MonadIO m, ShapePtr a s) => Attrib' s m Word64
group = liftStateVar $ H.group . toShape

elasticity :: (MonadIO m, ShapePtr a s) => Attrib' s m Float
elasticity = liftStateVarToFloat $ H.elasticity . toShape

friction :: (MonadIO m, ShapePtr a s) => Attrib' s m Float
friction = liftStateVarToFloat $ H.friction . toShape

surfaceVel :: (MonadIO m, ShapePtr a s) => Attrib' s m (V2 Float)
surfaceVel = liftStateVarToMappedFloat $ H.surfaceVel . toShape

localToWorld :: MonadIO m => Body a -> P2 Float -> m (P2 Float)
localToWorld (BWrap b) p = liftIO $ fmap realToFrac <$> H.localToWorld b (realToFrac <$> p)

worldToLocal :: MonadIO m => Body a -> P2 Float -> m (P2 Float)
worldToLocal (BWrap b) p = liftIO $ fmap realToFrac <$> H.worldToLocal b (realToFrac <$> p)
