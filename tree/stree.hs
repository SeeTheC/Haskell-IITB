data Stree a = Null | Fork a (Stree a) (Stree a) deriving (Show,Eq)

t1 = Fork 1 Null Null
t2 = Fork 10 (Fork 5 (Fork 2 Null Null) (Fork 6 Null Null)) ( Fork 13 Null Null)
t3 = Fork 10 (Fork 5 (Fork 2 Null (Fork 1 Null Null)) (Fork 9 (Fork 7 Null (Fork 8 Null Null)) Null)) ( Fork 13 Null Null)


treeHeight Null = 0
treeHeight (Fork _ Null Null) = 0
treeHeight (Fork a l r) = 1 + max (treeHeight l) (treeHeight r)

-- Inoder Traversal : L Root R
inorder  Null = []
inorder (Fork a l r) =  inorder l ++ [a]  ++ inorder r


-- portorder Traversal : L R Root
postorder  Null = []
postorder (Fork a l r) =  postorder l ++ postorder r ++ [a]

-- preorder Traversal : Root L R
preorder  Null = []
preorder (Fork a l r) =  [a] ++ preorder l ++ preorder r 

-- Returns the root of the tree
treeRoot (Fork a _ _) = a


-- Breath First Traversal
bfs Null = []
bfs t = bfs_aux (pushQ [] t)

bfs_aux q | q == [] = []
          | otherwise= [a] ++ bfs_aux updatedQ
            where (Fork a l r)=fetchQ q
                  updatedQ= if l /= Null && r /= Null
                        then pushQ (pushQ (popQ q) l) r
                        else if l /= Null 
                        then pushQ (popQ q) l
                        else if r /= Null 
                        then pushQ (popQ q) r
                        else (popQ q)

-- Creating queue
pushQ q a = q ++ [a]
popQ q | q == [] = []
       | otherwise= drop 1 q
fetchQ q = q !! 0 


-- flatten
flatten Null = []
flatten (Fork a l r) =  flatten l ++ [a] ++ flatten r

-- insert : Binary Search Tree format
insert x Null = Fork x Null Null
insert x (Fork a l r) | x < a  = Fork a (insert x l) r
                      | x == a = Fork a l r
                      | x > a  = Fork a l (insert x r)

-- member
member x Null = False
member x (Fork a l r) | x == a = True
                      | x < a = member x l
                      | x > a = member x r

-- delete node from tree.
-- Method: Find the node and Replace the node with is inoder successor

delete x Null = Null
delete x (Fork a l r) | x<a   = Fork a (delete x l) r
                      | x==a  = join l r
                      | x > a = Fork a l (delete x r)
                     
join l Null = l
join l r = Fork (ht r) l (tt r) 

ht t = head (flatten t)
-- deleting leftmost node.. i.e inoder succeror node o
tt (Fork a l r) | l == Null = r
                | otherwise = Fork a (tt l) r
