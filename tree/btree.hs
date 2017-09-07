-- in this tree data will be at leaf
data Btree a = Leaf a | Fork (Btree a) (Btree a) deriving Show

tree1 = Fork (Leaf 1) (Leaf 2)
tree2 = Fork (tree1) (Leaf 3)

treeHeight (Leaf _) = 0
treeHeight (Fork l r) = 1 + max (treeHeight l) (treeHeight r)

mirror n@(Leaf a) =n
mirror (Fork l r) = Fork (mirror r) (mirror l)  

-- Returns list
--flatten (Leaf a)=[a]
--flatten (Fork l r)= flatten (l) ++ flatten (r)

-- flatten in trail recursion form
flatten_tr t = flatten_tr' t []
               where flatten_tr' (Leaf a) rs = a : rs
                     flatten_tr' (Fork l r) rs = flatten_tr' l (flatten_tr' r rs)


