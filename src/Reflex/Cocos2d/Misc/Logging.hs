{-# LANGUAGE FlexibleContexts #-}
module Reflex.Cocos2d.Misc.Logging
  (
    debug
  , timedDebug

  , errorL
  , traceL
  , traceShowL
  ) where

import System.IO.Unsafe
import Data.Time
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Exception.Lifted

debug :: MonadIO m => String -> m ()
debug = undefined

-- inspired by Control.Logging from logging
timedDebug :: (MonadBaseControl IO m, MonadIO m) => String -> m a -> m a
timedDebug msg f = do
  start <- liftIO getCurrentTime
  res <- f `catch` \e -> do
    let str = show (e :: SomeException)
    dMsg start $ " (FAIL: " ++ str ++ ")"
    throwIO e
  dMsg start ""
  return res
  where dMsg start extraMsg = do
          end <- liftIO getCurrentTime
          debug $ msg ++ extraMsg ++ " [" ++ show (diffUTCTime end start) ++ "]"

errorL :: String -> a
errorL str = error (unsafePerformIO (debug str) `seq` str)

traceL :: String -> a -> a
traceL str expr = unsafePerformIO $ debug str >> return expr

traceShowL :: Show a => a -> a1 -> a1
traceShowL = traceL . show
