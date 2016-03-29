{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ImpredicativeTypes #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE GADTs #-}
-- A frp binding for Box2DWeb that can be used conveniently with Cocos2D
module Reflex.Cocos2d.Box2D
    (
      World
    , Body
    , Fixture
    , BodyType(..)
    , FixtureShape(..)
    , FixtureDef
    , friction
    , restitution
    , density
    , shape
    , BodyDef
    , HasBodyDef(..)
    , BodyConfig
    , setBodyType
    , setBodyPosition
    , setBodyRotation
    , setBodyVelocity
    , setBodyAngularVelocity
    , DynBody
    , WorldConfig
    , gravity
    , DynWorld
    , world
    , createFixture
    , createBody
    )
  where

import Diagrams
import Data.Default
import Data.Dependent.Sum (DSum (..))
import Data.Time.Clock
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Ref
import Control.Lens
import Control.Concurrent.MVar
import GHCJS.Types
import GHCJS.Marshal
import Reflex
import Reflex.Host.Class
import Reflex.Trans
import Reflex.Cocos2d.Utils
import Reflex.Cocos2d.Class

-- public types
newtype World = World JSVal
newtype Body = Body JSVal
newtype Fixture = Fixture JSVal

-- private types
newtype B2Vec = B2Vec JSVal
newtype B2Shape = B2Shape JSVal
newtype B2FixtureDef = B2FixtureDef JSVal
newtype B2BodyDef = B2BodyDef JSVal

---- FOREIGN IMPORTS ----
foreign import javascript unsafe "new Box2D.b2World(new Box2D.b2Vec2(0, 0))" b2_createWorld :: IO World
foreign import javascript unsafe "$1.SetGravity(new Box2D.b2Vec2($2, $3))" b2_setGravity :: World -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.Step($2)" b2_step :: World -> Double -> IO ()
foreign import javascript unsafe "$1.GetBodyList()" b2_getBodyList :: World -> IO Body
foreign import javascript unsafe "$1.CreateBody($2)" b2_createBody :: World -> B2BodyDef -> IO Body
foreign import javascript unsafe "$1.GetNext()" b2_getNext :: Body -> IO Body
foreign import javascript unsafe "$1.GetPosition()" b2_getPosition :: Body -> IO B2Vec
foreign import javascript unsafe "$1.GetAngle()" b2_getAngle :: Body -> IO Double
foreign import javascript unsafe "$1.SetType($2)" b2_setType :: Body -> Int -> IO ()
foreign import javascript unsafe "$1.SetPosition(new Box2D.b2Vec2($2, $3))" b2_setPosition :: Body -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.SetAngle($2)" b2_setAngle :: Body -> Double -> IO ()
foreign import javascript unsafe "$1.SetLinearVelocity(new Box2D.b2Vec2($2, $3))" b2_setLinearVelocity :: Body -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.SetAngularVelocity($2)" b2_setAngularVelocity :: Body -> Double -> IO ()
foreign import javascript unsafe "$1===$2" b2_eq :: Body -> Body -> Bool
-- Fixture
foreign import javascript unsafe "$1.CreateFixture($2)" b2_createFixture :: Body -> B2FixtureDef -> IO Fixture

-- BodyDef
foreign import javascript unsafe "new Box2D.b2BodyDef()" b2_createBodyDef :: IO B2BodyDef
foreign import javascript unsafe "$1.type = $2" b2bdef_setType :: B2BodyDef -> Int -> IO ()
foreign import javascript unsafe "$1.position.Set($2, $3)" b2bdef_setPosition :: B2BodyDef -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.angle = $2" b2bdef_setAngle :: B2BodyDef -> Double -> IO ()
foreign import javascript unsafe "$1.velocity.Set($2, $3)" b2bdef_setVelocity :: B2BodyDef -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.angularVelocity = $2" b2bdef_setAngularVelocity :: B2BodyDef -> Double -> IO ()

-- FixtureDef
foreign import javascript unsafe "new Box2D.b2FixtureDef()" b2_createFixtureDef :: IO B2FixtureDef
foreign import javascript unsafe "$1.friction = $2" b2fdef_setFriction :: B2FixtureDef -> Double -> IO ()
foreign import javascript unsafe "$1.restitution = $2" b2fdef_setRestitution :: B2FixtureDef -> Double -> IO ()
foreign import javascript unsafe "$1.density = $2" b2fdef_setDensity :: B2FixtureDef -> Double -> IO ()
foreign import javascript unsafe "$1.shape = $2" b2fdef_setShape :: B2FixtureDef -> B2Shape -> IO ()

-- B2Shape
foreign import javascript unsafe "new Box2D.b2PolygonShape()" b2_createPolygonShape :: IO B2Shape
foreign import javascript unsafe "new Box2D.b2CircleShape()" b2_createCircleShape :: IO B2Shape
foreign import javascript unsafe "$1.SetAsBox($2,$3)" b2p_poly_setAsBox :: B2Shape -> Double -> Double -> IO ()
-- width, height, center_x, center_y, angle
foreign import javascript unsafe "$1.SetAsBox($2,$3, new Box2D.b2Vec2($4, $5), $6)" b2s_poly_setAsBox_ :: B2Shape -> Double -> Double -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.SetAsArray($2, $2.length)" b2s_poly_setAsArray :: B2Shape -> JSVal -> IO ()
foreign import javascript unsafe "$1.m_radius = $2" b2s_cir_setRadius :: B2Shape -> Double -> IO ()
foreign import javascript unsafe "$1.m_p.SetV(new Box2D.b2Vec2($2, $3))" b2s_cir_setCenter :: B2Shape -> Double -> Double -> IO ()

-- Generics
foreign import javascript unsafe "new Box2D.b2Vec2($1, $2)" b2_createVec :: Double -> Double -> IO B2Vec
foreign import javascript unsafe "$1.x" b2_getX :: B2Vec -> IO Double
foreign import javascript unsafe "$1.y" b2_getY :: B2Vec -> IO Double

instance ToJSVal B2Vec where
    toJSVal (B2Vec v) = return v

-- | the BodyType, mapping to 0, 1, 2 respectively
data BodyType = Static | Kinematic | Dynamic deriving (Show, Read, Enum)

-- right now we don't care about center
data FixtureShape = Polygon [P2 Double]
                  | Circle (P2 Double) Double deriving (Show, Read)
makeLenses ''FixtureShape

data FixtureDef = FixtureDef { _friction :: Double -- ^ friction coefficient, usually in the range [0,1]
                             , _restitution :: Double -- ^ elasticity, usually in the range [0,1]
                             , _density :: Double -- ^ kg/m^2
                             , _shape :: FixtureShape
                             }
makeLenses ''FixtureDef

data BodyDef = BodyDef { _bodyType :: BodyType
                       , _bodyPosition :: P2 Double
                       , _bodyRotation :: Direction V2 Double
                       , _bodyVelocity :: V2 Double
                       -- | Angle rotated per second
                       , _bodyAngularVelocity :: Angle Double
                       -- | the fixture definitins, this allows a body to
                       -- be created with static fixtures
                       , _fixtureDefs :: [FixtureDef]
                       }
makeClassy ''BodyDef

instance Default BodyDef where
    def = BodyDef { _bodyType = Static
                  , _bodyPosition = 0
                  , _bodyRotation = xDir
                  , _bodyVelocity = 0
                  , _bodyAngularVelocity = 0 @@ rad
                  -- | the fixture definitins, this allows a body to
                  -- be created with static fixtures
                  , _fixtureDefs = []
                  }

data BodyConfig t = BodyConfig { _setBodyType :: Event t BodyType
                               , _setBodyPosition :: Event t (P2 Double)
                               , _setBodyRotation :: Event t (Direction V2 Double)
                               , _setBodyVelocity :: Event t (V2 Double)
                               -- | this is taken as degree/second to
                               -- comply with the same unit as in rotation
                               , _setBodyAngularVelocity :: Event t (Angle Double)
                               -- , setAwake :: Event t Bool
                               -- , setActive :: Event t Bool
                               , _bcToBodyDef :: BodyDef
                               }
makeLenses ''BodyConfig

instance Reflex t => Default (BodyConfig t) where
    def = BodyConfig { _setBodyType = never
                     , _setBodyPosition = never
                     , _setBodyRotation = never
                     , _setBodyVelocity = never
                     , _setBodyAngularVelocity = never
                     , _bcToBodyDef = def
                     }

instance HasBodyDef (BodyConfig t) where
    bodyDef = bcToBodyDef

instance Eq Body where
    (==) = b2_eq

data BodyTriggers t = BodyTriggers {
    _bTrBody :: Body, -- body reference variable
    _bTrPosition :: Ref IO (Maybe (EventTrigger t (P2 Double))),
    _bTrRotation :: Ref IO (Maybe (EventTrigger t (Direction V2 Double)))
}
makeLenses ''BodyTriggers


-- NOTE: the Dynamics contained inside is only accurate to the extent of
-- the polling interval - any change that happens within in the interval
-- would only be picked up in the next poll
data DynBody t = DynBody { _body :: Body -- ^ Used with Destroy()
                         , _dbToTrans :: Trans t
                         }
-- ! two DynBody are eqauted over the underlying Body references
instance Eq (DynBody t) where
    (DynBody b1 _) == (DynBody b2 _) = b1 == b2

makeLenses ''DynBody

instance HasTrans (DynBody t) t where
    trans = dbToTrans

data WorldConfig t = WorldConfig { _gravity :: Dynamic t (V2 Double)
                                 }
makeLenses ''WorldConfig

instance Reflex t => Default (WorldConfig t) where
    def = WorldConfig $ constDyn 0

data DynWorld t = DynWorld World (MVar [BodyTriggers t])

-- | Create a Box2D world that changes its gravity according to the
-- Dynamic, and updates on the given signal
world :: NodeGraph t m => WorldConfig t -> Event t NominalDiffTime -> m (DynWorld t)
world (WorldConfig gravity) ticks = do
    runWithActions <- askRunWithActions
    world <- liftIO b2_createWorld
    appDyn (\(V2 x y) -> liftIO $ b2_setGravity world x y) gravity
    bodyTrs :: MVar [BodyTriggers t] <- liftIO $ newMVar []
    forH_ ticks $ \dt -> liftIO $ do
      b2_step world (realToFrac dt)
      let process :: BodyTriggers t -> IO ()
          process (BodyTriggers b posTr rotTr) = do
              pos <- b2_getPosition b
              x <- b2_getX pos
              y <- b2_getY pos
              angle <- b2_getAngle b -- this is in radians
              sendV posTr (x ^& y)
              sendV rotTr (eDir $ angle @@ rad)
          -- sendV :: Ref IO (Maybe (EventTrigger t a)) -> a -> IO ()
          sendV tr v = do
            mt <- readRef tr
            forM_ mt $ \et -> runWithActions [et :=> Identity v]
      withMVar bodyTrs $ mapM_ process
    return $ DynWorld world bodyTrs

createFixture :: Body -> FixtureDef -> IO Fixture
createFixture body (FixtureDef fric rest density shape) = do
    fdef <- b2_createFixtureDef
    b2fdef_setFriction fdef fric
    b2fdef_setRestitution fdef rest
    b2fdef_setDensity fdef density
    sh <- case shape of
      Polygon verts -> do
        sh <- b2_createPolygonShape
        vs <- forM verts $ \p ->
              let (x, y) = unp2 p
              in b2_createVec x y
        jvs <- toJSVal vs
        b2s_poly_setAsArray sh jvs
        return sh
      Circle p radius -> do
        let (x, y) = unp2 p
        sh <- b2_createCircleShape
        b2s_cir_setCenter sh x y
        b2s_cir_setRadius sh radius
        return sh
    b2fdef_setShape fdef sh
    b2_createFixture body fdef

createBody :: NodeGraph t m => DynWorld t -> BodyConfig t -> m (DynBody t)
createBody (DynWorld world bodyTrs) (BodyConfig te pe re ve ave (BodyDef t p r (V2 vx vy) av fs)) = do
    let (x, y) = unp2 p
    -- first assemble the BodyDef
    body <- liftIO $ do
      bdef <- b2_createBodyDef
      b2bdef_setType bdef (fromEnum t)
      b2bdef_setPosition bdef x y
      b2bdef_setAngle bdef (r^._theta.rad)
      b2bdef_setVelocity bdef vx vy
      b2bdef_setAngularVelocity bdef (av^.rad)
      b2_createBody world bdef
    liftIO . forM_ fs $ createFixture body
    -- set body attributes on each new event
    forH_ te $ liftIO . b2_setType body . fromEnum
    forH_ pe $ \p -> let (x, y) = unp2 p in liftIO (b2_setPosition body x y)
    forH_ re $ liftIO . b2_setAngle body . (^._theta.rad)
    forH_ ve $ \(V2 x y) -> liftIO (b2_setLinearVelocity body x y)
    forH_ ave $ liftIO . b2_setAngularVelocity body . (^.rad)
    -- create trigger refs
    (posE, posTr) <- newEventWithTriggerRef
    (rotE, rotTr) <- newEventWithTriggerRef
    -- store the trigger refs into the user data (export)
    liftIO . modifyMVar_ bodyTrs $ return . (BodyTriggers body posTr rotTr:)
    -- finally return the events
    posDyn <- holdDyn p posE
    rotDyn <- holdDyn r rotE
    return $ DynBody body (Trans posDyn rotDyn)

