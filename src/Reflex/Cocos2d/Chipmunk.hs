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

    , Shape(..)

    , Fixture
    , shape
    , mass
    , sensor
    , CollisionCoefficients
    , HasCollisionCoefficients(..)

    , StaticBodyConfig
    , HasStaticBodyConfig(..)

    , DynamicBodyConfig
    , bodyVel
    , setBodyVel
    , bodyAngularVel
    , setBodyAngularVel
    , active
    , setActive
    , force
    , impulse

    , SpaceConfig
    , HasSpaceConfig(..)

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
    , contactPoints

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

import Diagrams hiding (normal)
import Data.List
import Data.Default
import Data.Time.Clock
import Data.Dependent.Sum (DSum (..))
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.Foreign.Callback
import Reflex
import Reflex.Host.Class
import Reflex.Trans
import Reflex.Cocos2d.Utils
import Reflex.Cocos2d.Class

-- public types
newtype Space = Space JSVal deriving (FromJSVal)
class IsSpace a where
    toSpace :: a -> Space

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
foreign import javascript unsafe "$1.setVel($2)" cp_setVel :: Body -> CPVec -> IO ()
foreign import javascript unsafe "$1.setAngle($2)" cp_setAngle :: Body -> Double -> IO ()
foreign import javascript unsafe "$1.setAngVel($2)" cp_setAngVel :: Body -> Double -> IO ()
foreign import javascript unsafe "$1.applyForce($2, $3)" cp_applyForce :: Body -> CPVec -> CPVec -> IO ()
foreign import javascript unsafe "$1.applyImpulse($2, $3)" cp_applyImpulse :: Body -> CPVec -> CPVec -> IO ()
foreign import javascript unsafe "$1.activate()" cp_activate :: Body -> IO ()
foreign import javascript unsafe "$1.sleep()" cp_sleep :: Body -> IO ()
foreign import javascript unsafe "$1.data = { collisionType:0, began:null, postSolve:null, separate:null }" cp_addCollisionData :: Body -> IO ()
foreign import javascript unsafe "$1.data.collisionType = $2" cp_setCollisionType :: Body -> Int -> IO ()
foreign import javascript unsafe "$1.data.began = $2" cp_setCollisionBegan :: Body -> Callback a -> IO ()
foreign import javascript unsafe "$1.data.postSolve = $2" cp_setCollisionPostSolve :: Body -> Callback a -> IO ()
foreign import javascript unsafe "$1.data.separate = $2" cp_setCollisionSeparate :: Body -> Callback a -> IO ()
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

unCPVec :: CPVec -> IO (Double, Double)
unCPVec v = (,) <$> cp_getX v <*> cp_getY v

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
data StaticBodyConfig t a = StaticBodyConfig { _bodyPos :: P2 Double
                                             , _setBodyPos :: Event t (P2 Double)
                                             , _bodyRot :: Direction V2 Double
                                             , _setBodyRot :: Event t (Direction V2 Double)
                                             , _collisionType :: a
                                             , _setCollisionType :: Event t a
                                             , _fixtures :: [Fixture]
                                             }
makeClassy ''StaticBodyConfig

instance (Reflex t, Enum a) => Default (StaticBodyConfig t a) where
    def = StaticBodyConfig { _bodyPos = 0
                           , _setBodyPos = never
                           , _bodyRot = xDir
                           , _setBodyRot = never
                           , _collisionType = toEnum 0
                           , _setCollisionType = never
                           , _fixtures = []
                           }

data DynamicBodyConfig t a = DynamicBodyConfig { _dbToRBC :: StaticBodyConfig t a
                                               , _bodyVel :: V2 Double -- ^ velocity
                                               , _setBodyVel :: Event t (V2 Double)
                                               , _bodyAngularVel :: Double -- ^ radians/s
                                               , _setBodyAngularVel :: Event t Double
                                               , _active :: Bool -- ^ activate/deactivate
                                               , _setActive :: Event t Bool
                                               -- apply force (V2 Double) at point (P2 Double)
                                               , _force :: Event t (V2 Double, P2 Double)
                                               -- apply impulse (V2 Double) at point (P2 Double)
                                               , _impulse :: Event t (V2 Double, P2 Double)
                                               }
makeLenses ''DynamicBodyConfig

instance (Reflex t, Enum a) => Default (DynamicBodyConfig t a) where
    def = DynamicBodyConfig { _dbToRBC = def
                            , _bodyVel = 0
                            , _setBodyVel = never
                            , _bodyAngularVel = 0
                            , _setBodyAngularVel = never
                            , _active = True
                            , _setActive = never
                            , _force = never
                            , _impulse = never
                            }

instance HasStaticBodyConfig (DynamicBodyConfig t a) t a where
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

instance IsSpace (DynSpace t) where
    toSpace (DynSpace sp _) = sp

space :: NodeGraph t m => Event t NominalDiffTime -> SpaceConfig t -> m (DynSpace t)
space ts conf = do
    space <- liftIO cp_createSpace
    liftIO $ cp_addCollisionHandler space >> cp_setIterations space (conf ^. iterations)
    appDyn (liftIO . (cp_setGravity space <=< r2ToCPVec)) (conf ^. gravity)
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
       contactP <- uncurry (^&) <$> (cp_getContactPoint a >>= unCPVec)
       normal <- uncurry (^&) <$> (cp_getNormal a >>= unCPVec)
       depth <- cp_getDist a
       return . Just $ ContactPoint contactP normal depth


data Arbiter a = Arbiter { _aToCoP :: CollisionCoefficients
                         , _surfaceVel :: V2 Double
                         , _otherBody :: Body
                         , _otherCollisionType :: a
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

-- -- XXX: Note, different invocations of this functions inside the same
-- -- program should still use the same Enum type to make sure there is no
-- -- crossovers
-- collisions :: (Enum a, IsSpace s, NodeGraph t m) => s -> a -> a -> m (CollisionEvents t)
-- collisions space collisionTypeA collisionTypeB = do
--     let sp = toSpace space
--         ctA = fromEnum collisionTypeA
--         ctB = fromEnum collisionTypeB
--         convCallback :: (Arbiter -> Space -> IO ()) -> IO (Callback (JSVal -> JSVal -> IO ()), IO ())
--         convCallback h = do
--           cb <- syncCallback2 ThrowWouldBlock $ \a b -> join . fmap sequence_ $ liftM2 h <$> fromJSVal a <*> fromJSVal b
--           return $ (cb, releaseCallback cb)
--     runWithActions <- askRunWithActions
--     evt <- newEventWithTrigger $ \et -> do
--       (began, releaseBegan) <- convCallback $ \arb _ -> do
--         runWithActions [et :=> Identity (Just arb, Nothing, Nothing)]
--       (postSolve, releasePostSolve) <- convCallback $ \arb _ -> do
--         runWithActions [et :=> Identity (Nothing, Just arb, Nothing)]
--       (separate, releaseSeparate) <- convCallback $ \arb _ -> do
--         runWithActions [et :=> Identity (Nothing, Nothing, Just arb)]
--       cp_addCollisionHandler sp ctA ctB began postSolve separate
--       return $ releaseBegan >> releasePostSolve >> releaseSeparate
--     return CollisionEvents { _collisionBegan = fmapMaybe (^._1) evt
--                            , _collisionPostSolve = fmapMaybe (^._2) evt
--                            , _collisionSeparate = fmapMaybe (^._3) evt
--                            }


data DynBody t a = DynBody { _cpBody :: Body -- ^ Used for some reference related management
                           , _dbToTrans :: Trans t
                           , _dbToCe :: CollisionEvents t a
                           }
makeLenses ''DynBody

instance IsBody (DynBody t a) where
    toBody (DynBody b _ _) = b

instance Eq (DynBody t a) where
    (DynBody b1 _ _) == (DynBody b2 _ _) = b1 == b2

instance HasTrans (DynBody t a) t where
    trans = dbToTrans

instance HasCollisionEvents (DynBody t a) t a where
    collisionEvents = dbToCe


calcMoment :: Double -> Shape -> IO Double
calcMoment mass (Circle rad os) = cp_momentForCircle mass 0 rad <$> r2ToCPVec os
calcMoment mass (Segment _ a b) = cp_momentForSegment mass <$> r2ToCPVec a <*> r2ToCPVec b
calcMoment mass (Poly verts) = cp_momentForPoly mass <$> toJSVal (cpFlattenedVerts verts)

-- flatten vertices and reverse them to be clockwise order
cpFlattenedVerts :: [P2 Double] -> [Double]
cpFlattenedVerts verts = foldl' (\a p -> p^._x:p^._y:a) [] verts

staticBody :: (NodeGraph t m, Enum a) => DynSpace t -> StaticBodyConfig t a -> m (DynBody t a)
staticBody (DynSpace space steps) (StaticBodyConfig pos setPosE rot setRotE ct setCt fixs) = do
    body <- liftIO $ do
      totalMoment <- fmap sum . forM fixs $ \f -> calcMoment (f^.mass) (f^.shape)
      let totalMass = sumOf (traverse.mass) fixs
      cp_createBody totalMass totalMoment
    liftIO $ do
      cp_addCollisionData body
      cp_setPos body =<< r2ToCPVec pos
      cp_setAngle body (rot^._theta.rad)
      cp_setCollisionType body $ fromEnum ct
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
    sequenceH_ . ffor setPosE $ liftIO . (cp_setPos body <=< r2ToCPVec)
    sequenceH_ . ffor setRotE $ liftIO . cp_setAngle body . (^._theta.rad)
    sequenceH_ . ffor setCt $ liftIO . cp_setCollisionType body . fromEnum
    -- create the triggers for position and rotation
    ets <- forG steps . const . liftIO $ do
      pos <- cp_getPos body
      (x, y) <- unCPVec pos
      angle <- cp_getAngle body
      return (x ^& y, eDir $ angle @@ rad)
    let (posE, rotE) = splitE ets
    posDyn <- holdDyn pos posE
    rotDyn <- holdDyn rot rotE
    runWithActions <- askRunWithActions
    let convCallback ffi et = do
          cb <- syncCallback3 ThrowWouldBlock $ \a bodyV ctV -> do
            e <- cp_arbGetElasticity a
            u <- cp_arbGetFriction a
            sv <- uncurry V2 <$> (cp_arbGetSurfaceVel a >>= unCPVec)
            mcps <- cp_arbGetContactPointSet a >>= fromJSVal
            mct <- fromJSVal ctV
            case (mcps, mct) of
              (Just cps, Just t) -> runWithActions [
                                        et :=> Identity Arbiter { _aToCoP = CollisionCoefficients e u
                                                                , _surfaceVel = sv
                                                                , _otherBody = Body bodyV
                                                                , _otherCollisionType = toEnum t
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
    return $ DynBody body (Trans posDyn rotDyn) (CollisionEvents began postSolve separate)

dynamicBody :: (NodeGraph t m, Enum a, Eq a) => DynSpace t -> DynamicBodyConfig t a -> m (DynBody t a)
dynamicBody dspace@(DynSpace space _) (DynamicBodyConfig sconf vel setVelE angVel setAngVel active setActiveE force impulse) = do
    dbody@(DynBody body _ _) <- staticBody dspace sconf
    liftIO $ do
      cp_setVel body =<< r2ToCPVec vel
      cp_setAngVel body angVel
      -- actually add the body
      if active then cp_addBody space body
                else return ()
    sequenceH_ . ffor setVelE $ liftIO . (cp_setVel body <=< r2ToCPVec)
    sequenceH_ . ffor setAngVel $ liftIO . cp_setAngle body
    sequenceH_ . ffor force $ \(f, point) -> liftIO . join $ cp_applyForce body <$> r2ToCPVec f <*> r2ToCPVec point
    sequenceH_ . ffor impulse $ \(i, point) -> liftIO . join $ cp_applyImpulse body <$> r2ToCPVec i <*> r2ToCPVec point
    -- TODO: when the chipmunk Mac jsb bug is fixed we should change to
    -- cp_smartAdd/cp_smartRemove
    sequenceH_ . ffor setActiveE $ \a -> liftIO $ if a then cp_smartAdd space body
                                                       else cp_smartRemove space body
    return dbody

