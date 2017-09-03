import Data.List
-- (**) Generate the combinations of K distinct objects chosen from the N elements of a list

combinations n l | n == 1 = [ [x] | x<- l]
                 | otherwise = cSameLevel n l

-- Find remaing list by taking the first element then find pair for size n-1 ++ removing first element then find size n pair for remaining list
-- Its like element is in the pair or element is not in the pair	  
cSameLevel _ [] = []
cSameLevel n l  = map (head l:) (combinations (n-1) (tail l) ) ++  cSameLevel n (tail l)



-- Implementing above logic using tail(s)


-- its tails NOT tail
combinations1 n l | n ==0 = [[]]
                  | otherwise= [ x:e | x:xs<-tails l, e<- combinations1 (n-1) xs] 
