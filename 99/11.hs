-- Problem 11
--Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

data Encode n a = Single a | Multiple n a deriving (Show,Eq)
encodeModified l =  foldr  (\a o-> (encode a):o) [] el
                    where el=encodeModified_aux l
                          encode (n,x)= if n==1 then Single x else Multiple n x
encodeModified_aux [] = []
encodeModified_aux l  = reverse ( foldl (\ ((n,c):t) x-> if c == x then (n+1,c):t else [(1,x)]++[(n,c)]++t ) [(1,head l)] (tail l) )
