data Gtree a = Gnode a [Gtree a] deriving (Show,Eq)
data Btree a = Bnode (Btree a) (Btree a) | Leaf a deriving (Show,Eq)


g1 = Gnode 'f' [Gnode 'g' [Gnode 'x' [], Gnode 'l' []],Gnode 'h' [Gnode 'l' []],Gnode '5' []]
b1 = Bnode (Bnode (Bnode (Leaf 'f') (Bnode (Bnode (Leaf 'g') (Leaf 'x')) (Leaf 'l'))) (Bnode (Leaf 'h') (Leaf 'l'))) (Leaf '5')

g2 = Gnode 'f' [Gnode '5' [],Gnode '6' [],Gnode '7' []]
b2 = Bnode (Bnode (Bnode (Leaf 'f') (Leaf '5')) (Leaf '6')) (Leaf '7')

b3 = Bnode (Leaf 'g') (Leaf 'x')
b4 = Bnode (Bnode (Leaf 'g') (Leaf 'x')) (Leaf 'l')

gtob (Gnode c xs) | null xs = Leaf c
                  | otherwise = foldl combine (Leaf c) xs
                    where combine t node = Bnode (t) (gtob node)



btog (Leaf a) = Gnode a []
btog (Bnode lt rt) = Gnode lc (xs++[rnode])
                     where (Gnode lc xs) = btog lt
                           rnode = btog rt
