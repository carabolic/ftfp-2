module Data.AList
    (
      AList
    , nil
    , put
    , isIn
    , get
    , del
    , isNil
    ) where

type AList a = [(Char, a)]

nil :: AList a
nil = []

put :: Char -> a -> AList a -> AList a
put key value []   = [(key, value)]
put key value ((cK, cV):xs) = if key == cK
                                    then (cK, value):xs
                                    else (cK, cV):put key value xs

isIn :: Char -> AList a -> Bool
isIn _ [] = False
isIn c ((x, _):xs) = c == x || isIn c xs

get :: Char -> AList a -> Maybe a
get _ [] = Nothing
get c ((k, v):xs) = if c == k then Just v else get c xs

del :: Char -> AList a -> AList a
del _ [] = []
del c ((k, v):xs) = if c == k then xs else (k, v) : del c xs

isNil :: AList a -> Bool
isNil [] = True
isNil _  = False