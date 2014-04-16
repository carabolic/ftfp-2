module ListenFunktional where

f1 :: [Integer] -> [Integer]
f1 l = f1' [] l
    where
        f1' l []         = reverse l
        f1' l (x:xs)
            | x `isIn` l = f1' l xs
            | otherwise = f1' (x:l) xs
        isIn x [] = False
        isIn x (y:ys) = if x == y then True else isIn x ys

newF1 :: [Integer] -> [Integer]
newF1 = foldl (\b a -> if a `elem` b then b else b ++ [a]) []

f2 :: Double -> [Double] -> Double
f2 x []     = 0
f2 x (a:as) = a + x * (f2 x as)

newF2 :: Double -> [Double] -> Double
newF2 x coeffs = foldl (\result b -> b + result * x) 0 $ reverse coeffs

f3 :: [Integer] -> Maybe Integer
f3 []     = Nothing
f3 (x:xs) = Just $ f3' xs x
    where
        f3' [] a        = a
        f3' (x:xs) a
            | a < x     = f3' xs x
            | otherwise = f3' xs a

newF3 :: [Integer] -> Maybe Integer
newF3 []     = Nothing
newF3 (x:xs) = Just $ foldl max x xs

f4 :: [a] -> [a]
f4 []     = []
f4 (x:xs) = f4 xs ++ [x]

newF4 :: [a] -> [a]
newF4 = reverse

f5 :: (a -> b -> c) -> [a] -> [b] -> [c]
f5 f [] _          = []
f5 f _ []          = []
f5 f (x:xs) (y:ys) = f x y : f5 f xs ys

newF5 :: (a -> b -> c) -> [a] -> [b] -> [c]
newF5 = zipWith
