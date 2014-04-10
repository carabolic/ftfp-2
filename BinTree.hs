module BinTree (
    BinTree,
    insert,
    insertList,
    greatest,
    inorder,
	) where

data BinTree a = Nil | Node (BinTree a) a (BinTree a)
				deriving (Show)

-- | Insert an element to the binary tree using the supplied comparator.
insert :: (a -> a -> Bool) -> a -> BinTree a -> BinTree a
insert _ e Nil = Node Nil e Nil
insert p e (Node left c right) = if p e c
									then Node (insert p e left) c right
									else Node left c (insert p e right)

insertList :: (a -> a -> Bool) -> [a] -> BinTree a -> BinTree a
insertList p es t = foldl (flip $ insert p) t es

greatest :: BinTree a -> Maybe a
greatest (Node _ e Nil)   = Just e
greatest Nil              = Nothing
greatest (Node _ _ right) = greatest right

inorder :: BinTree a -> [a]
inorder Nil = []
inorder (Node Nil e right)  = e:(inorder right)
inorder (Node left e right) = inorder left ++ [e] ++ inorder right
