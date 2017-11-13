-- Generate all posible tree of node n
data Tree a = Empty | Branch a (Tree a) (Tree a)  deriving (Show, Eq)

-- This will generate all trees
generateAllTree:: Integer->[Tree Char]
generateAllTree 0 = [Empty]
generateAllTree n = [ Branch 'x' left right  |  i<-[0..(n-1)], left<-generateTree (i), right<-generateTree (n-i-1)] 


-- complete binary tree
cbTree:: Integer->[Tree Char]
cbTree 0 = [Empty]
cbTree n = [ Branch 'x' left right  |  i<-[0..(n-1)], left<-generateTree (i), right<-generateTree (n-i-1)] 

