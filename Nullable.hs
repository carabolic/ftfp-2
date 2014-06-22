module Nullable where

import Control.Fixpoint
import qualified Data.Set as S

type Nonterminal = String
type Terminal = String
type Grammar = S.Set (Nonterminal, [Symbol])

data Symbol = N Nonterminal | T Terminal deriving (Eq, Ord, Show)

nullable :: Grammar -> S.Set Symbol
nullable grammar = condFixpoint f w
    where
        f = indirectEps grammar
        w = eps grammar

nullable' :: Grammar -> S.Set Symbol
nullable' grammar = fixpoint (nullableStep grammar)

nullableStep :: Grammar -> S.Set Symbol -> S.Set Symbol
nullableStep g s = if S.null s then eps g else indirectEps g s

eps :: Grammar -> S.Set Symbol
eps = S.map (N . fst) . S.filter (null . snd)

indirectEps :: Grammar -> S.Set Symbol -> S.Set Symbol
indirectEps g s = s `S.union` s'
    where
        -- true if all symbols are nullable nonterminals
        isNullable (_, xs) = all (`S.member` s) xs
        s' = S.map (N . fst) $ S.filter isNullable g

testGrammar :: Grammar
testGrammar = S.fromList
    [
      ("Z", [T "d"])
    , ("Z", [N "X", N "Y", N "Z"])
    , ("Y", [])
    , ("Y", [T "d"])
    , ("X", [N "Y"])
    , ("X", [T "a"])
    ]