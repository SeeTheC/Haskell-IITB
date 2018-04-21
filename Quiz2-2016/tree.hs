data Btree a = Leaf a | Fork (Btree a) (Btree a) deriving (Show)


inorder (Leaf a) = [a]
inorder (Fork lt rt) = lt ++ rt

data (Ord a) => Stree a = Null | Fork a (Stree a) (Stree a) deriving Show

insert x Null = Fork x Null Null
insert x n@(Fork a lt rt) | x==a = n 
                          | x < a = Fork a (insert x lt) rt
                          | otherwise  = Fork a lt (insert x rt)

