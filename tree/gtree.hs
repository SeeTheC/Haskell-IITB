-- General Tree
data Gtree a = Node a [Gtree a] deriving (Show,Eq)

t1 = Node 1 []
t2 = Node 1 [ Node 2 [], Node 3 []]
t3 = Node 1 [ Node 2 [Node 5 [],Node 6 []],  Node 3 [], Node 4 [Node 7 [],Node 8 [],Node 9 []]]
t4 = Node 2 [ Node 3 [], Node 4 []]

-- Map for tree
mapTree f (Node a l) = Node (f a) (map (mapTree f) l)


-- flatten : Here it will act as  PreOrder traversal
flatten (Node a l) = [a] ++ concat (map flatten l)


-- fold Tree

foldTree g f id (Node a l) = g a (foldr f id (map (foldTree g f id) l) ) 
 

