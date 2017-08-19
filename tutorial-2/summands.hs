summand n  = ( summand_aux (findPair n) [] ) ++ [[n]]
-- concatenate  2-pair list with 3 pair list then   
summand_aux [] r   =  r
summand_aux l1 r   =  (summand_aux l2 r) ++ l1 ++ r
                      where l2 = (splitHead l1)

-- finding pair for head of every list i.e for [[4,1]] o/p : [ [3,1,1],[2,3,1],[1,3,1] ] 
splitHead l1 = removeOuterList ( [ map (\x-> x ++ (tail a) ) (findPair (head a) ) | a <- l1, (head a) > 1 ] )

-- Spit n into 2 pair sum i.e  i/p 5 : o/p :  [ [4,1],[3,2],[2,3],[1,4] ]
findPair 1  =  [[]]
findPair n  =  findPair_aux n (n-1) 1 []
findPair_aux n a b r | a==1 = ([a,b]:r)
                     | otherwise = findPair_aux n (a-1) (b+1) ([a,b]:r)  

-- i/p: removeOuterList [[[1,2]],[[3]]] o/p [[1,2],[3]]
removeOuterList [] = []
removeOuterList (x:[]) = x
removeOuterList (x:xs) = x ++ (removeOuterList xs)




-- Method 2 : Not Vey Efficient. Repeative computation 
summands1 1 = [[1]]
summands1 n = [  a:el | a<-[1..(n-1)], el <-(summands1 (n-a)) ] ++ [[n]]


