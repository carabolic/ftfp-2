module Reachable where

import Data.Graph
import Control.Fixpoint

import qualified Data.Set as S

reachableFD :: Graph -> Vertex -> S.Set Vertex
reachableFD g s = condFixpointFD start more step (S.singleton s)
    where
        start vs = (vs, vs)
        more = not . S.null . snd
        step (vs, ws) = let d = S.findMin ws
                            vs' = S.insert d vs
                            ws' = (ws `S.union` outSet g d) `S.difference` vs'
                        in (vs', ws')
