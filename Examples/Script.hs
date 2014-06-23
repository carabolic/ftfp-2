module Examples.Script where

import Control.Script

fac :: Integer -> Script Val
fac n = do x $:= num n
           y $:= num 1
           while (var x $> num 0)
                 (do y $:= var y $* var x
                     x $:= var x $- num 1)
           var y
    where { x = "x"; y = "y" }

add :: Integer -> Integer -> Script Val
add n m = do x $:= num n
             y $:= num m
             var x $+ var y
    where { x = "x"; y = "y" }
