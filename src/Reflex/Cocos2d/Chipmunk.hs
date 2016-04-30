{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE JavaScriptFFI #-}
module Reflex.Cocos2d.Chipmunk
    (
      Space

    , Body
    , IsBody(..)

    , Shape(..)

    , Fixture(Fixture)
    , shape
    , mass
    , elasticity
    , friction
    , sensor

    , StaticBodyConfig
    , HasStaticBodyConfig(..)

    , DynamicBodyConfig
    , bodyVel
    , setBodyVel
    , bodyAngularVel
    , setBodyAngularVel
    , setActive
    , force
    , impulse

    , SpaceConfig(SpaceConfig)
    , HasSpaceConfig(..)

    , Step
    , stepTime

    , DynSpace
    , steps
    , space

    , DynBody

    , staticBody
    , dynamicBody
    )
  where

import Diagrams
import Data.List
import Data.Default
import Data.Time.Clock
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import GHCJS.Marshal
import GHCJS.Types
import Reflex
import Reflex.Trans
import Reflex.Cocos2d.Utils
import Reflex.Cocos2d.Class

-- public types
newtype Space = Space JSVal
newtype Body = Body JSVal
class IsBody a where
    toBody :: a -> Body

instance IsBody Body where
    toBody = id

instance Eq Body where
    (==) = cp_eq

-- private types
newtype CPShape = CPShape JSVal
newtype CPVec = CPVec JSVal

---- Space ----
foreign import javascript unsafe "new cp.Space()" cp_createSpace :: IO Space
foreign import javascript unsafe "$1.setIterations($2)" cp_setIterations :: Space -> Int -> IO ()
foreign import javascript unsafe "$1.gravity = $2" cp_setGravity :: Space -> CPVec -> IO ()
-- time is in seconds; step the world and run all the trigger functions on all bodies
-- THIS ASSUMES that each body has its userdata set to a trigger function
foreign import javascript unsafe "$1.step($2)" cp_step :: Space -> Double -> IO ()
foreign import javascript unsafe "$1.addBody($2)" cp_addBody :: Space -> Body -> IO ()
foreign import javascript unsafe "$1.addShape($2)" cp_addShape :: Space -> CPShape -> IO ()

---- Body ----
-- mass, moment
foreign import javascript unsafe "new cp.Body($1, $2)" cp_createBody :: Double -> Double -> IO Body
foreign import javascript unsafe "$1.setPos($2)" cp_setPos :: Body -> CPVec -> IO ()
foreign import javascript unsafe "$1.setVel($2)" cp_setVel :: Body -> CPVec -> IO ()
foreign import javascript unsafe "$1.setAngle($2)" cp_setAngle :: Body -> Double -> IO ()
foreign import javascript unsafe "$1.setAngVel($2)" cp_setAngVel :: Body -> Double -> IO ()
foreign import javascript unsafe "$1.applyForce($2, $3)" cp_applyForce :: Body -> CPVec -> CPVec -> IO ()
foreign import javascript unsafe "$1.applyImpulse($2, $3)" cp_applyImpulse :: Body -> CPVec -> CPVec -> IO ()
foreign import javascript unsafe "$1.activate()" cp_activate :: Body -> IO ()
foreign import javascript unsafe "$1.sleep()" cp_sleep :: Body -> IO ()
foreign import javascript unsafe "if (!$2.space) { if ($1.isLocked()) { $1.addPostStepCallback(function() { $1.addBody($2); }); } else {  $1.addBody($2); } }" cp_smartAdd :: Space -> Body -> IO ()
foreign import javascript unsafe "if ($2.space) { if ($1.isLocked()) { $1.addPostStepCallback(function() { $1.removeBody($2); }); } else {  $1.removeBody($2); } }" cp_smartRemove :: Space -> Body -> IO ()
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
foreign import javascript unsafe "$1===$2" cp_eq :: Body -> Body -> Bool
foreign import javascript unsafe "cp.v($1, $2)" cp_v :: Double -> Double -> IO CPVec
foreign import javascript unsafe "$1.x" cp_getX :: CPVec -> IO Double
foreign import javascript unsafe "$1.y" cp_getY :: CPVec -> IO Double
-- mass, inner_radius, outer_radius, (offset_x, offset_y)
foreign import javascript unsafe "cp.momentForCircle($1, $2, $3, $4)" cp_momentForCircle :: Double -> Double -> Double -> CPVec -> Double
foreign import javascript unsafe "cp.momentForSegment($1, $2, $3)" cp_momentForSegment :: Double -> CPVec -> CPVec -> Double
foreign import javascript unsafe "cp.momentForPoly($1, $2, cp.vzero)" cp_momentForPoly :: Double -> JSVal -> Double

r2ToCPVec :: R2 t => t Double -> IO CPVec
r2ToCPVec p = cp_v (p^._x) (p^._y)

data Shape = Circle Double (P2 Double)
           | Segment Double (P2 Double) (P2 Double)
           -- NOTE: the list of points need to be in ANTI-clockwise order,
           -- and in convex shape
           | Poly [P2 Double] deriving (Show, Read)
-- wrapper type around Shape to unify the interface
data Fixture = Fixture { _shape :: Shape
                       , _mass :: Double
                       , _elasticity :: Double
                       , _friction :: Double
                       , _sensor :: Bool
                       } deriving (Show, Read)
makeLenses ''Fixture

instance Default Fixture where
    def = Fixture { _shape = Poly []
                  , _mass =  1/0 -- infinity
                  , _elasticity = 0
                  , _friction = 0
                  , _sensor = False
                  }

-- due to the confusion between static & rogue bodies, we decide to use
-- rogue bodies for all static bodies (and call them *static* bodies)
data StaticBodyConfig t = StaticBodyConfig { _bodyPos :: P2 Double
                                           , _setBodyPos :: Event t (P2 Double)
                                           , _bodyRot :: Direction V2 Double
                                           , _setBodyRot :: Event t (Direction V2 Double)
                                           , _fixtures :: [Fixture]
                                           }
makeClassy ''StaticBodyConfig

instance Reflex t => Default (StaticBodyConfig t) where
    def = StaticBodyConfig { _bodyPos = 0
                           , _setBodyPos = never
                           , _bodyRot = xDir
                           , _setBodyRot = never
                           , _fixtures = []
                           }

data DynamicBodyConfig t = DynamicBodyConfig { _dbToRBC :: StaticBodyConfig t
                                             , _bodyVel :: V2 Double -- ^ velocity
                                             , _setBodyVel :: Event t (V2 Double)
                                             , _bodyAngularVel :: Double -- ^ radians/s
                                             , _setBodyAngularVel :: Event t Double
                                             , _setActive :: Event t Bool -- ^ activate/deactivate
                                             -- apply force (V2 Double) at point (P2 Double)
                                             , _force :: Event t (V2 Double, P2 Double)
                                             -- apply impulse (V2 Double) at point (P2 Double)
                                             , _impulse :: Event t (V2 Double, P2 Double)
                                             }
makeLenses ''DynamicBodyConfig

instance Reflex t => Default (DynamicBodyConfig t) where
    def = DynamicBodyConfig { _dbToRBC = def
                            , _bodyVel = 0
                            , _setBodyVel = never
                            , _bodyAngularVel = 0
                            , _setBodyAngularVel = never
                            , _setActive = never
                            , _force = never
                            , _impulse = never
                            }

instance HasStaticBodyConfig (DynamicBodyConfig t) t where
    staticBodyConfig = dbToRBC

data SpaceConfig t = SpaceConfig { _iterations :: Int
                                 , _gravity :: Dynamic t (V2 Double)
                                 }
makeClassy ''SpaceConfig

instance Reflex t => Default (SpaceConfig t) where
    def = SpaceConfig { _iterations = 10
                      , _gravity = constDyn 0
                      }

data Step = Step { _stepTime :: NominalDiffTime
                 }
makeLenses ''Step

data DynSpace t = DynSpace { _cpSpace :: Space
                           , _steps :: Event t Step
                           }
makeLenses ''DynSpace

space :: NodeGraph t m => Event t NominalDiffTime -> SpaceConfig t -> m (DynSpace t)
space ts conf = do
    space <- liftIO cp_createSpace
    liftIO $ cp_setIterations space (conf ^. iterations)
    appDyn (liftIO . (cp_setGravity space <=< r2ToCPVec)) (conf ^. gravity)
    fmap (DynSpace space) . forH ts $ \dt -> liftIO $ do
      cp_step space $ realToFrac dt
      return $ Step dt

data DynBody t = DynBody { _cpBody :: Body -- ^ Used for some reference related management
                         , _dbToTrans :: Trans t
                         }
makeLenses ''DynBody

instance IsBody (DynBody t) where
    toBody (DynBody b _) = b

instance Eq (DynBody t) where
    (DynBody b1 _) == (DynBody b2 _) = b1 == b2

instance HasTrans (DynBody t) t where
    trans = dbToTrans

calcMoment :: Double -> Shape -> IO Double
calcMoment mass (Circle rad os) = cp_momentForCircle mass 0 rad <$> r2ToCPVec os
calcMoment mass (Segment _ a b) = cp_momentForSegment mass <$> r2ToCPVec a <*> r2ToCPVec b
calcMoment mass (Poly verts) = cp_momentForPoly mass <$> toJSVal (cpFlattenedVerts verts)

-- flatten vertices and reverse them to be clockwise order
cpFlattenedVerts :: [P2 Double] -> [Double]
cpFlattenedVerts verts = foldl' (\a p -> p^._x:p^._y:a) [] verts

staticBody :: NodeGraph t m => DynSpace t -> StaticBodyConfig t -> m (DynBody t)
staticBody (DynSpace space steps) (StaticBodyConfig pos setPosE rot setRotE fixs) = do
    body <- liftIO $ do
      totalMoment <- fmap sum . forM fixs $ \f -> calcMoment (f^.mass) (f^.shape)
      let totalMass = sumOf (traverse.mass) fixs
      cp_createBody totalMass totalMoment
    liftIO $ do
      cp_setPos body =<< r2ToCPVec pos
      cp_setAngle body (rot^._theta.rad)
      -- add all the shapes
      forM_ fixs $ \(Fixture shape _ elas fric sensor) -> do
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
    forH_ setPosE $ liftIO . (cp_setPos body <=< r2ToCPVec)
    forH_ setRotE $ liftIO . cp_setAngle body . (^._theta.rad)
    -- create the triggers for position and rotation
    ets <- forH steps . const . liftIO $ do
      pos <- cp_getPos body
      x <- cp_getX pos
      y <- cp_getY pos
      angle <- cp_getAngle body
      return (x ^& y, eDir $ angle @@ rad)
    let (posE, rotE) = splitE ets
    posDyn <- holdDyn pos posE
    rotDyn <- holdDyn rot rotE
    return $ DynBody body (Trans posDyn rotDyn)

dynamicBody :: NodeGraph t m => DynSpace t -> DynamicBodyConfig t -> m (DynBody t)
dynamicBody dspace@(DynSpace space _) (DynamicBodyConfig sconf vel setVelE angVel setAngVel active force impulse) = do
    dbody@(DynBody body _) <- staticBody dspace sconf
    liftIO $ do
      cp_setVel body =<< r2ToCPVec vel
      cp_setAngVel body angVel
      -- actually add the body
      cp_addBody space body
    forH_ setVelE $ liftIO . (cp_setVel body <=< r2ToCPVec)
    forH_ setAngVel $ liftIO . cp_setAngle body
    -- TODO: when the chipmunk Mac jsb bug is fixed we should change to
    -- cp_smartAdd/cp_smartRemove
    forH_ active $ \a -> liftIO $ if a then cp_activate body
                                       else cp_sleep body
    forH_ force $ \(f, point) -> liftIO $ cp_applyForce body <$> r2ToCPVec f <*> r2ToCPVec point
    forH_ impulse $ \(i, point) -> liftIO $ cp_applyImpulse body <$> r2ToCPVec i <*> r2ToCPVec point
    return dbody
