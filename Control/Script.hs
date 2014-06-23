module Control.Script where

import Prelude hiding (subtract)
import qualified Data.Map as M

type ErrMsg = String
type Store = M.Map Variable Val
type Variable = String
type Statement = Script Val

data Val = Void | Number Integer | Boolean Bool deriving (Eq)

instance Show Val where
  show Void        = "()"
  show (Number n)  = show n
  show (Boolean b) = show b

data Script a = Script (Store -> (Either ErrMsg a, Store))

instance Functor Script where
  fmap f (Script stmt) = Script (\s -> let (ret, s') = stmt s
                                           ret' = fmap f ret
                                       in  (ret', s'))

instance Monad Script where
  return x = Script (\s -> (Right x, s))
  Script stmt >>= f = Script (\s -> let (ret, s') = stmt s
                                        Script stmt' = either logError f ret
                                    in  stmt' s')

logError :: String -> Script a
logError errMsg = Script (\s -> (Left errMsg, s))

-- | No operation.
nop :: Statement
nop = return Void

-- | Dereference Variable.
var :: Variable -> Statement
var x = Script (\s -> (value x s, s))
  where
    value v s = maybe (Left $ v ++ " is not defined") Right (M.lookup v s)

-- | Integer constant.
num :: Integer -> Script Val
num = return . Number

-- | Boolean constant.
bool :: Bool -> Script Val
bool = return . Boolean

infixl 6 $+
-- | Addition
($+) :: Script Val -> Script Val -> Script Val
s1 $+ s2 = do add' <- fmap add s1
              s2' <- s2
              return (add' s2')
  where
    add (Number n) (Number m) = Number $ n + m

infixl 6 $-
-- | Subtraction
($-) :: Script Val -> Script Val -> Script Val
s1 $- s2 = do subtract' <- fmap subtract s1
              s2' <- s2
              return (subtract' s2')
  where
    subtract (Number m) (Number n) = Number $ m - n

infixl 7 $*
-- | Multiplication
($*) :: Script Val -> Script Val -> Script Val
s1 $* s2 = do multiply' <- fmap multiply s1
              s2'       <- s2
              return (multiply' s2')
  where
    multiply (Number m) (Number n) = Number $ m * n

infixl 7 $/
-- | Integer division
-- TODO add error handling for division by zero
($/) :: Script Val -> Script Val -> Script Val
s1 $/ s2 = do divide' <- fmap divide s1
              s2'     <- s2
              return (divide' s2')
  where
    divide (Number m) (Number n) = Number $ m `div` n

infix 4 $:=
-- | Assignment
($:=) :: Variable -> Script Val -> Script Val
x $:= script = script >>= addToStore
  where
    addToStore :: Val -> Script Val
    addToStore val = Script (\s -> (Right Void, M.insert x val s))

infix 5 $==
-- | Check for equality
($==) :: Script Val -> Script Val -> Script Val
s1 $== s2 = do equals' <- fmap (==) s1
               s2'     <- s2
               bool (equals' s2')

infix 5 $<
-- | Less than
($<) :: Script Val -> Script Val -> Script Val
s1 $< s2 = do lessThan' <- fmap lessThan s1
              s2'       <- s2
              return (lessThan' s2')
  where
    lessThan (Number m) (Number n) = Boolean (m < n)
    lessThan (Boolean m) (Boolean n) = Boolean (m < n)

infix 5 $>
-- | Greater than
($>) :: Script Val -> Script Val -> Script Val
s1 $> s2 = ((s1 $< s2) $== bool False) $== ((s1 $== s2) $== bool False)

-- | While loop (head controlled)
while :: Script Val -> Script Val -> Script Val
while p body = do (Boolean p') <- p
                  if p'
                    then body >> while p body
                    else nop

-- | Conditional execution
--
-- @cond a b c@
cond :: Script Val -> Script Val -> Script Val -> Script Val
cond p ifStmt elseStmt = do (Boolean p') <- p
                            if p' then ifStmt else elseStmt

exec :: Script Val -> String
exec (Script stmt) = result
  where
    -- TODO print store (i.e. variable assignments) as well
    (ret, _) = stmt M.empty
    result = case ret of
      Left errMsg -> "error: " ++ errMsg
      Right val   -> show val

