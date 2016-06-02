{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE JavaScriptFFI #-}
module Reflex.Cocos2d.Chipmunk
    (
      Space

    , Body
    , IsBody(..)
    , DynamicBody
    , IsDynamicBody(..)

    , Shape(..)

    , Fixture
    , shape
    , mass
    , sensor
    , CollisionCoefficients
    , HasCollisionCoefficients(..)

    , ContactPoint
    , contact
    , contactNormal
    , depth

    , CollisionEvents
    , HasCollisionEvents(..)

    , Arbiter
    , surfaceVel
    , otherBody
    , otherCollisionType
    , thisCollisionType
    , contactPoints

    , Step
    , stepTime

    , DynSpace
    , steps
    , space

    , DynBody
    , transDyn

    , staticBody
    , dynamicBody

    -- attrs --
    , iterations
    , gravity
    , collisionType
    , vel
    , angularVel
    , active
    , force
    , impulse
    )
  where

import Linear
import Diagrams hiding (normal)
import Data.List
import Data.Default
import Data.Time.Clock
import Data.Dependent.Sum (DSum (..))
import Control.Lens hiding (set)
import Control.Monad
import Control.Monad.IO.Class
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.Foreign.Callback
import Reflex
import Reflex.Host.Class
import Reflex.Cocos2d.Attributes
import Reflex.Cocos2d.Node
import Reflex.Cocos2d.Class

-- public types
newtype Space = Space JSVal deriving (FromJSVal)

class IsSpace a where
    toSpace :: a -> Space

instance IsSpace Space where
    toSpace = id

newtype Body = Body JSVal

class IsBody a where
    toBody :: a -> Body

instance IsBody Body where
    toBody = id

instance Eq Body where
    Body a == Body b = a `js_eq` b

newtype DynamicBody = DynamicBody JSVal

class IsBody a => IsDynamicBody a where
    toDynamicBody :: a -> DynamicBody

instance IsDynamicBody DynamicBody where
    toDynamicBody = id

instance IsBody DynamicBody where
    toBody (DynamicBody v) = Body v

instance Eq DynamicBody where
    DynamicBody a == DynamicBody b = a `js_eq` b

-- private types
newtype CPShape = CPShape JSVal
newtype CPVec = CPVec JSVal

---- Space ----
foreign import javascript unsafe "new cp.Space()" cp_createSpace :: IO Space
foreign import javascript unsafe "$1.setIterations($2)" cp_setIterations :: Space -> Int -> IO ()
foreign import javascript unsafe "$1.iterations" cp_getIterations :: Space -> IO Int
foreign import javascript unsafe "$1.gravity = $2" cp_setGravity :: Space -> CPVec -> IO ()
foreign import javascript unsafe "$1.gravity" cp_getGravity :: Space -> IO CPVec
-- time is in seconds; step the world and run all the trigger functions on all bodies
-- THIS ASSUMES that each body has its userdata set to a trigger function
foreign import javascript unsafe "$1.step($2)" cp_step :: Space -> Double -> IO ()
foreign import javascript unsafe "$1.addBody($2)" cp_addBody :: Space -> Body -> IO ()
foreign import javascript unsafe "$1.addShape($2)" cp_addShape :: Space -> CPShape -> IO ()
foreign import javascript unsafe "var beganFunc=function(a,s){ var bA=a.getA().body; var bB=a.getB().body; if (bA.data.began){ bA.data.began(a,bB,bB.data.collisionType); } if (bB.data.began){ bB.data.began(a,bA,bA.data.collisionType); } return true; }; var postSolveFunc=function(a,s){ var bA=a.getA().body; var bB=a.getB().body; if (bA.data.postSolve){ bA.data.postSolve(a,bB,bB.data.collisionType); } if (bB.data.postSolve){ bB.data.postSolve(a,bA,bA.data.collisionType); }}; var separateFunc=function(a,s){ var bA=a.getA().body; var bB=a.getB().body; if (bA.data.separate){ bA.data.separate(a,bB,bB.data.collisionType); } if (bB.data.separate){ bB.data.separate(a,bA,bA.data.collisionType); }}; $1.addCollisionHandler(0, 0, beganFunc, null, postSolveFunc, separateFunc)" cp_addCollisionHandler :: Space -> IO ()

---- Arbiter ----
foreign import javascript unsafe "$1.e" cp_arbGetElasticity :: JSVal -> IO Double
foreign import javascript unsafe "$1.u" cp_arbGetFriction :: JSVal -> IO Double
foreign import javascript unsafe "$1.surface_vr" cp_arbGetSurfaceVel :: JSVal -> IO CPVec
foreign import javascript unsafe "$1.getContactPointSet()" cp_arbGetContactPointSet :: JSVal -> IO JSVal
---- Contact Point ----
foreign import javascript unsafe "$1.point" cp_getContactPoint :: JSVal -> IO CPVec
foreign import javascript unsafe "$1.normal" cp_getNormal :: JSVal -> IO CPVec
foreign import javascript unsafe "$1.dist" cp_getDist :: JSVal -> IO Double

---- Body ----
-- mass, moment
foreign import javascript unsafe "new cp.Body($1, $2)" cp_createBody :: Double -> Double -> IO Body
foreign import javascript unsafe "$1.setPos($2)" cp_setPos :: Body -> CPVec -> IO ()
foreign import javascript unsafe "$1.setVel($2)" cp_setVel :: DynamicBody -> CPVec -> IO ()
foreign import javascript unsafe "$1.getVel()" cp_getVel :: DynamicBody -> IO CPVec
foreign import javascript unsafe "$1.setAngle($2)" cp_setAngle :: Body -> Double -> IO ()
foreign import javascript unsafe "$1.setAngVel($2)" cp_setAngVel :: DynamicBody -> Double -> IO ()
foreign import javascript unsafe "$1.getAngVel()" cp_getAngVel :: DynamicBody -> IO Double
foreign import javascript unsafe "$1.applyForce($2, $3)" cp_applyForce :: DynamicBody -> CPVec -> CPVec -> IO ()
foreign import javascript unsafe "$1.applyImpulse($2, $3)" cp_applyImpulse :: DynamicBody -> CPVec -> CPVec -> IO ()
foreign import javascript unsafe "$1.activate()" cp_activate :: DynamicBody -> IO ()
foreign import javascript unsafe "$1.sleep()" cp_sleep :: DynamicBody -> IO ()
foreign import javascript unsafe "$1.data = { space:$2, collisionType:0, began:null, postSolve:null, separate:null }" cp_setupData :: Body -> Space -> IO ()
foreign import javascript unsafe "$1.data.collisionType = $2" cp_setCollisionType :: Body -> Int -> IO ()
foreign import javascript unsafe "$1.data.collisionType" cp_getCollisionType :: Body -> IO Int
foreign import javascript unsafe "$1.data.began = $2" cp_setCollisionBegan :: Body -> Callback a -> IO ()
foreign import javascript unsafe "$1.data.postSolve = $2" cp_setCollisionPostSolve :: Body -> Callback a -> IO ()
foreign import javascript unsafe "$1.data.separate = $2" cp_setCollisionSeparate :: Body -> Callback a -> IO ()
foreign import javascript unsafe "$1.isRogue()" cp_isRogue :: DynamicBody -> IO Bool
foreign import javascript unsafe "$1.data.space" cp_getSpaceFromData :: DynamicBody -> IO Space
foreign import javascript unsafe "if (!$2.space) { if ($1.isLocked()) { $1.addPostStepCallback(function() { $1.addBody($2); }); } else {  $1.addBody($2); } }" cp_smartAdd :: Space -> DynamicBody -> IO ()
foreign import javascript unsafe "if ($2.space) { if ($1.isLocked()) { $1.addPostStepCallback(function() { $1.removeBody($2); }); } else {  $1.removeBody($2); } }" cp_smartRemove :: Space -> DynamicBody -> IO ()
-- get body realtime info
foreign import javascript unsafe "$1.p" cp_getPos :: Body -> IO CPVec
foreign import javascript unsafe "$1.a" cp_getAngle :: Body -> IO Double

---- Shape ----
-- body, verts: [x1,y1,x2,y2,x3,y3,...], (offset_x, offset_y)
foreign import javascript unsafe "new cp.PolyShape($1, $2, cp.vzero)" cp_createPolyShape :: Body -> JSVal -> IO CPShape
-- body, radius, (offset_x, offset_y)
foreign import javascript unsafe "new cp.CircleShape($1, $2, $3)" cp_createCircleShape :: Body -> Double -> CPVec -> IO CPShape
-- body, (a_x, a_y), (b_x, b_y), radius
foreign import javascript unsafe "new cp.SegmentShape($1, $2, $3, $4)" cp_createSegment :: Body -> CPVec -> CPVec -> Double -> IO CPShape
foreign import javascript unsafe "$1.setElasticity($2)" cp_setElasticity :: CPShape -> Double -> IO ()
foreign import javascript unsafe "$1.setFriction($2)" cp_setFriction :: CPShape -> Double -> IO ()
foreign import javascript unsafe "$1.setSensor($2)" cp_setSensor :: CPShape -> Bool -> IO ()

---- Misc ----
foreign import javascript unsafe "$1===$2" js_eq :: JSVal -> JSVal -> Bool
foreign import javascript unsafe "cp.v($1, $2)" cp_v :: Double -> Double -> IO CPVec
foreign import javascript unsafe "$1.x" cp_getX :: CPVec -> IO Double
foreign import javascript unsafe "$1.y" cp_getY :: CPVec -> IO Double
-- mass, inner_radius, outer_radius, (offset_x, offset_y)
foreign import javascript unsafe "cp.momentForCircle($1, $2, $3, $4)" cp_momentForCircle :: Double -> Double -> Double -> CPVec -> Double
foreign import javascript unsafe "cp.momentForSegment($1, $2, $3)" cp_momentForSegment :: Double -> CPVec -> CPVec -> Double
foreign import javascript unsafe "cp.momentForPoly($1, $2, cp.vzero)" cp_momentForPoly :: Double -> JSVal -> Double

r2ToCPVec :: R2 t => t Double -> IO CPVec
r2ToCPVec p = cp_v (p^._x) (p^._y)

cpVecToR2 :: (PrevDim c ~ Double, FinalCoord c ~ Double, Coordinates c) => CPVec -> IO c
cpVecToR2 v = (^&) <$> cp_getX v <*> cp_getY v

data Shape = Circle Double (P2 Double)
           | Segment Double (P2 Double) (P2 Double)
           -- NOTE: the list of points need to be in ANTI-clockwise order,
           -- and in convex shape
           | Poly [P2 Double] deriving (Show, Read)

data CollisionCoefficients = CollisionCoefficients { _elasticity :: Double
                                                   , _friction :: Double
                                                   } deriving (Show, Read)
makeClassy ''CollisionCoefficients

instance Default CollisionCoefficients where
    def = CollisionCoefficients 0 0

-- wrapper type around Shape to unify the interface
data Fixture = Fixture { _shape :: Shape
                       , _mass :: Double
                       , _fToCoP :: CollisionCoefficients
                       , _sensor :: Bool
                       } deriving (Show, Read)
makeLenses ''Fixture

instance HasCollisionCoefficients Fixture where
    collisionCoefficients = fToCoP

instance Default Fixture where
    def = Fixture { _shape = Poly []
                  , _mass =  1/0 -- infinity
                  , _fToCoP = def
                  , _sensor = False
                  }

-- due to the confusion between static & rogue bodies, we decide to use
-- rogue bodies for all static bodies (and call them *static* bodies)
data Step = Step { _stepTime :: NominalDiffTime
                 }
makeLenses ''Step

data DynSpace t = DynSpace { _cpSpace :: Space
                           , _steps :: Event t Step
                           }
makeLenses ''DynSpace

instance IsSpace (DynSpace t) where
    toSpace (DynSpace sp _) = sp

space :: NodeGraph t m => Event t NominalDiffTime -> [Prop Space m] -> m (DynSpace t)
space ts props = do
    space <- liftIO cp_createSpace
    liftIO $ cp_addCollisionHandler space
    set space props
    fmap (DynSpace space) . forG ts $ \dt -> liftIO $ do
      cp_step space $ realToFrac dt
      return $ Step dt

-- collision handling
data ContactPoint = ContactPoint { _contact :: P2 Double
                                 , _contactNormal :: V2 Double
                                 , _depth :: Double
                                 } deriving (Show, Read)
makeLenses ''ContactPoint

instance FromJSVal ContactPoint where
    fromJSVal a = do
       contactP <- cp_getContactPoint a >>= cpVecToR2
       normal <- cp_getNormal a >>= cpVecToR2
       depth <- cp_getDist a
       return . Just $ ContactPoint contactP normal depth


data Arbiter a = Arbiter { _aToCoP :: CollisionCoefficients
                         , _surfaceVel :: V2 Double
                         , _otherBody :: Body
                         , _otherCollisionType :: a
                         , _thisCollisionType :: a
                         , _contactPoints :: [ ContactPoint ]
                         -- support for shapes?
                         }
makeLenses ''Arbiter

instance HasCollisionCoefficients (Arbiter a) where
    collisionCoefficients = aToCoP

data CollisionEvents t a = CollisionEvents { _collisionBegan :: Event t (Arbiter a)
                                           , _collisionPostSolve :: Event t (Arbiter a)
                                           , _collisionSeparate :: Event t (Arbiter a)
                                           }
makeClassy ''CollisionEvents

-- | DynBody is a body wrapped with Dynamic reflex components
data DynBody t a b = DynBody { _cpBody :: b -- ^ Used for some reference related management
                             , _transDyn :: Dynamic t (P2 Double, Direction V2 Double)
                             , _dbToCe :: CollisionEvents t a
                             }
makeLenses ''DynBody

instance IsBody b => IsBody (DynBody t a b) where
    toBody (DynBody b _ _) = toBody b

instance IsDynamicBody b => IsDynamicBody (DynBody t a b) where
    toDynamicBody (DynBody b _ _) = toDynamicBody b

instance Eq b => Eq (DynBody t a b) where
    (DynBody b1 _ _) == (DynBody b2 _ _) = b1 == b2

instance HasCollisionEvents (DynBody t a b) t a where
    collisionEvents = dbToCe


calcMoment :: Double -> Shape -> IO Double
calcMoment mass (Circle rad os) = cp_momentForCircle mass 0 rad <$> r2ToCPVec os
calcMoment mass (Segment _ a b) = cp_momentForSegment mass <$> r2ToCPVec a <*> r2ToCPVec b
calcMoment mass (Poly verts) = cp_momentForPoly mass <$> toJSVal (cpFlattenedVerts verts)

-- flatten vertices and reverse them to be clockwise order
cpFlattenedVerts :: [P2 Double] -> [Double]
cpFlattenedVerts verts = foldl' (\a p -> p^._x:p^._y:a) [] verts

initBody :: (NodeGraph t m, Enum a)
         => DynSpace t
         -> [Fixture]
         -> (DynBody t a Body -> m b) -> m b
initBody (DynSpace space steps) fixs setup = do
    body <- liftIO $ do
      totalMoment <- fmap sum . forM fixs $ \f -> calcMoment (f^.mass) (f^.shape)
      let totalMass = sumOf (traverse.mass) fixs
      cp_createBody totalMass totalMoment
    liftIO $ do
      cp_setupData body space
      -- add all the shapes
      forM_ fixs $ \(Fixture shape _ (CollisionCoefficients elas fric) sensor) -> do
        cps <- case shape of
          Circle rad os -> cp_createCircleShape body rad =<< r2ToCPVec os
          Segment rad a b -> do
            a' <- r2ToCPVec a
            b' <- r2ToCPVec b
            cp_createSegment body a' b' rad
          Poly verts -> cp_createPolyShape body =<< toJSVal (cpFlattenedVerts verts)
        -- set up all the remaining details
        cp_setElasticity cps elas
        cp_setFriction cps fric
        cp_setSensor cps sensor
        cp_addShape space cps
    rec let dbody = DynBody body tDyn (CollisionEvents began postSolve separate)
        res <- setup dbody
        currPos <- get body pos
        currRot <- get body rot
        -- create the triggers for position and rotation
        ets <- forG steps . const . liftIO $ do
          pos <- cp_getPos body >>= cpVecToR2
          angle <- cp_getAngle body
          return (pos, eDir $ angle @@ rad)
        tDyn <- holdDyn (currPos, currRot) ets
        runWithActions <- askRunWithActions
        let convCallback ffi et = do
              cb <- syncCallback3 ThrowWouldBlock $ \a bodyV ctV -> do
                e <- cp_arbGetElasticity a
                u <- cp_arbGetFriction a
                sv <- cp_arbGetSurfaceVel a >>= cpVecToR2
                mcps <- cp_arbGetContactPointSet a >>= fromJSVal
                mct <- fromJSVal ctV
                bct <- cp_getCollisionType body
                case (mcps, mct) of
                  (Just cps, Just t) -> runWithActions [
                                            et :=> Identity Arbiter { _aToCoP = CollisionCoefficients e u
                                                                    , _surfaceVel = sv
                                                                    , _otherBody = Body bodyV
                                                                    , _otherCollisionType = toEnum t
                                                                    , _thisCollisionType = toEnum bct
                                                                    , _contactPoints = cps
                                                                    }
                                        ]
                  _ -> return ()
              _ <- ffi cb
              return $ releaseCallback cb
        -- set up the collision handlers
        began <- newEventWithTrigger $ convCallback (cp_setCollisionBegan body)
        postSolve <- newEventWithTrigger $ convCallback (cp_setCollisionPostSolve body)
        separate <- newEventWithTrigger $ convCallback (cp_setCollisionSeparate body)
    return res

staticBody :: (NodeGraph t m, Enum a)
           => DynSpace t
           -> [Fixture] -> [Prop (DynBody t a Body) m]
           -> m (DynBody t a Body)
staticBody dspace fixs props = initBody dspace fixs $ \db -> set db props >> return db

dynamicBody :: (NodeGraph t m, Enum a, Eq a)
            => DynSpace t
            -> [Fixture] -> [Prop (DynBody t a DynamicBody) m]
            -> m (DynBody t a DynamicBody)
dynamicBody dspace fixs props = initBody dspace fixs $ \db -> do
        let (DynBody (Body bv) tDyn ce) = db
        -- XXX: cast to DynamicBody
        let db' = DynBody (DynamicBody bv) tDyn ce
        set db' props
        return db'

---- attributes ----
-- Space
iterations :: (MonadIO m, IsSpace sp) => Attrib sp m Int
iterations = attrib getter setter
  where getter sp = liftIO $ cp_getIterations (toSpace sp)
        setter sp iter = liftIO $ cp_setIterations (toSpace sp) iter

gravity :: (MonadIO m, IsSpace sp) => Attrib sp m (V2 Double)
gravity = attrib getter setter
  where getter sp = liftIO $ cp_getGravity (toSpace sp) >>= cpVecToR2
        setter sp g = liftIO $ r2ToCPVec g >>= cp_setGravity (toSpace sp)

-- Body

bodyPosAttr :: (MonadIO m, IsBody b) => Attrib b m (P2 Double)
bodyPosAttr = attrib getter setter
  where getter b = liftIO $ cp_getPos (toBody b) >>= cpVecToR2
        setter b v = liftIO $ r2ToCPVec v >>= cp_setPos (toBody b)

bodyRotAttr :: (MonadIO m, IsBody b) => Attrib b m (Direction V2 Double)
bodyRotAttr = attrib getter setter
    where getter b = liftIO $ eDir . (@@ rad) <$> cp_getAngle (toBody b)
          setter b d = liftIO $ cp_setAngle (toBody b) (d^._theta.rad)

instance {-# OVERLAPPING #-} MonadIO m => HasPosition Body m where
    pos = bodyPosAttr

instance {-# OVERLAPPING #-} MonadIO m => HasRotation Body m where
    rot = bodyRotAttr

instance {-# OVERLAPPING #-} MonadIO m => HasPosition DynamicBody m where
    pos = bodyPosAttr

instance {-# OVERLAPPING #-} MonadIO m => HasRotation DynamicBody m where
    rot = bodyRotAttr

instance {-# OVERLAPPING #-} (MonadIO m, IsBody b) => HasPosition (DynBody t a b) m where
    pos = bodyPosAttr

instance {-# OVERLAPPING #-} (MonadIO m, IsBody b) => HasRotation (DynBody t a b) m where
    rot = bodyRotAttr

collisionType :: (MonadIO m, Enum a, IsBody b) => Attrib (DynBody t a b) m a
collisionType = attrib getter setter
  where getter b = liftIO $ toEnum <$> cp_getCollisionType (toBody b)
        setter b ct = liftIO $ cp_setCollisionType (toBody b) (fromEnum ct)

vel :: (MonadIO m, IsDynamicBody b) => Attrib b m (V2 Double)
vel = attrib getter setter
  where getter b = liftIO $ cp_getVel (toDynamicBody b) >>= cpVecToR2
        setter b v = liftIO $ r2ToCPVec v >>= cp_setVel (toDynamicBody b)

angularVel :: (MonadIO m, IsDynamicBody b) => Attrib b m Double
angularVel = attrib getter setter
  where getter b = liftIO $ cp_getAngVel (toDynamicBody b)
        setter b = liftIO . cp_setAngVel (toDynamicBody b)

active :: (MonadIO m, IsDynamicBody b) => Attrib b m Bool
active = attrib getter setter
  where getter b = liftIO $ not <$> cp_isRogue (toDynamicBody b)
        setter b a = liftIO $ do
          let b' = toDynamicBody b
          sp <- cp_getSpaceFromData b'
          case a of
            True -> cp_smartAdd sp b'
            False -> cp_smartRemove sp b'

-- | Currently modelled as applying a force at the given point whenever set
force :: (MonadIO m, IsDynamicBody b) => SetOnlyAttrib b m (V2 Double, P2 Double)
force = SetOnlyAttrib' $ \b (f, p) -> liftIO $ do
          let b' = toDynamicBody b
          join $ cp_applyForce b' <$> r2ToCPVec f <*> r2ToCPVec p

impulse :: (MonadIO m, IsDynamicBody b) => SetOnlyAttrib b m (V2 Double, P2 Double)
impulse = SetOnlyAttrib' $ \b (i, p) -> liftIO $ do
            let b' = toDynamicBody b
            join $ cp_applyImpulse b' <$> r2ToCPVec i <*> r2ToCPVec p
