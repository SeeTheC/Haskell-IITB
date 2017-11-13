--(**) Construct height-balanced binary trees
-- In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the height of its right -- subtree are almost equal, which means their difference is not greater than one.

data Tree a = Empty | Branch a (Tree a) (Tree a)  deriving (Show, Eq)

hbalTree 0 = [(Empty,0)]
hbalTree 1 = [(Branch 'c' Empty Empty,1)]
-- generating all posible tree and then checking the height
hbalTree n = [ (Branch 'c' lt rt, (max lh rh) + 1 ) |  i<-[0..n-1],(lt,lh)<-hbalTree i,(rt,rh)<-hbalTree (n-i-1),abs(lh-rh)<=1  ] 



hbalTree1 x = map fst . hbalTree'
    where hbalTree' 0 = [(Empty, 0)]
          hbalTree' 1 = [(Branch x Empty Empty, 1)]
          hbalTree' n =
                let t = hbalTree' (n-2) ++ hbalTree' (n-1)
                in  [(Branch x lb rb, h) | (lb,lh) <- t, (rb,rh) <- t
                                         , let h = 1 + max lh rh, h == n]
