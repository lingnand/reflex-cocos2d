module Control.Monad.Random.Extra (
  pick
) where

import Control.Monad.Random

pick :: MonadRandom m => [(Double, a)] -> m a
pick dist = do
    let tot = sum $ fst <$> dist
        pk _ [] = error "No choice left"
        pk c ((p, a):xs) | c <= p' = a
                         | otherwise = pk (c-p') xs
          where p' = p/tot
    v <- getRandomR (0, 1)
    return $ pk v dist

