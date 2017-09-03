-- Group the elements of a set into disjoint subsets.
--a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.
-- Example
-- group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
-- (altogether 1260 solutions)


import Data.List

-- This will return both pair and remaining element of list
-- \\ difference operation
combination n l | n==0 = [([],[])]
                | otherwise= [ (x:p, l \\ (x:p))  | x:xs <- tails l ,  (p,q)<-combination (n-1) xs  ]
         


group1 p l | p == [] =[[]]
          | otherwise = [ l:a  | (l,nls)<-combination (head p) l,  a<-group1 (tail p) nls ]
