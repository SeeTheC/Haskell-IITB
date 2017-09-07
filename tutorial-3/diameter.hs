data Tree a = Node (Tree a) a (Tree a) | Null deriving Show

t = Node (Null) 1 (Null)
t0= Node (Node (Null) 2 (Null)) 1 (Node (Null) 3 (Null))
-- dia: 4
t1 = (Node  ( Node ( Node (Null) 4 (Null) ) 2 (Node (Null) 5 (Null)) ) 1 ( Node (Null) 3 (Null) ) )
-- diameter : 9
t2 = (Node  ( Node ( Node (Null) 4 (Null) ) 2 (Node (Node (Null) 6 (Null)) 5 (Node (Null) 7 (Null))) ) 1 ( Node (Null) 3 (Node (Null) 8 (Node (Node (Node (Null) 12 (Null)) 10 (Node (Null) 13 (Null))) 9 (Node (Null) 11 (Null)))) ) )
--diameter
t3= Node (Node (Node (Node (Null) 6 (Null)) 4 (Node (Node (Node (Null) 9 (Null)) 8 (Node (Null) 10 (Null))) 7 (Null))) 2 (Node (Null) 5 (Node (Node (Null) 12 (Null)) 11 (Node (Null) 13 (Node (Null) 14 (Null)))))) 1 (Node (Null) 3 (Node (Null) 15 (Null)))

-- for finding diameter taking height of leave is 1
height Null = 0	
height (Node l a r) = 1 + max (height l) (height r)

dia Null = 0
dia tp@(Node l a r) = maximum [ dia l , dia r, dia_aux tp]

-- diameter considering it is passing through the root 
dia_aux (Node l a r) = 1 + (height l) + (height r)
