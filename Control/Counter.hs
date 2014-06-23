module Control.Counter where

import Prelude hiding (mapM)

data Counter a = CM {eval :: Integer -> (a, Integer)}

instance Monad Counter where
    return a = CM (\n -> (a, n))
    CM m >>= f = CM (\n -> let (a, n1) = m n
                               CM m2   = f a
                           in  m2 n1)

-- | Oberves a counter and returns the value only and neglects the state (counter).
observe :: Counter a -> Integer -> a
observe cm = fst . eval cm 

set :: Integer -> Counter ()
set n = CM (\_ -> (() ,n))

tick :: Counter ()
tick = CM (\n -> ((), n + 1))

current :: Counter Integer
current = CM (\n -> (n, n))

next :: Counter Integer
next = do c <- current
          tick
          return c

numListM :: [a] -> Counter [(Integer, a)]
numListM [] = return []
numListM (x:xs) = do n <- current
                     tick
                     xs' <- numListM xs
                     return $ (n, x) : xs'

mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM _ []     = return []
mapM f (x:xs) = do x'  <- f x
                   xs' <- mapM f xs
                   return (x' : xs')

numListM' :: [a] -> Counter [(Integer, a)]
numListM' = mapM f
    where
        f c = do {n <- next; return (n, c)}
