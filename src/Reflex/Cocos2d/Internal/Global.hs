module Reflex.Cocos2d.Internal.Global
    (
      globalDirector
    , globalScheduler
    , globalEventDispatcher
    , globalTextureCache
    )
  where

import System.IO.Unsafe

import Graphics.UI.Cocos2d.Director
import Graphics.UI.Cocos2d.Texture

-- we can do these things because we assume by the time these are used, all
-- the necessary setup has already been performed in the app

{-# NOINLINE globalDirector #-}
globalDirector :: Director
globalDirector = unsafePerformIO director_getInstance

{-# NOINLINE globalScheduler #-}
globalScheduler :: Scheduler
globalScheduler = unsafePerformIO $ director_getScheduler globalDirector

{-# NOINLINE globalEventDispatcher #-}
globalEventDispatcher :: EventDispatcher
globalEventDispatcher = unsafePerformIO $ director_getEventDispatcher globalDirector

{-# NOINLINE globalTextureCache #-}
globalTextureCache :: TextureCache
globalTextureCache = unsafePerformIO $ director_getTextureCache globalDirector
