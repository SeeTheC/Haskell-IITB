lcs  [] _  = []
lcs  _ []  = [] 
lcs (x:xs) (y:ys) | x==y = (x) : (lcs xs ys) 
                    | otherwise= if (length l1) <= (length l2)
                               then l2
                               else l1
                               where l1= (lcs (x:xs) ys )
                                     l2= (lcs xs (y:ys) ) 
