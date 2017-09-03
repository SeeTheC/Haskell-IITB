-- Sorting a list of lists according to length of sublists
-- a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their
-- length. E.g. short lists first, longer lists later, or vice versa.

quickSort [] = []
quickSort (x:xs) = quickSort [ a | a<-xs,a<=x]  ++ [x] ++ quickSort [ a | a<-xs,a>x]

lsort [] = []
lsort (x:xs) = lsort [ a | a<-xs , length a<= length x] ++  [x] ++ lsort [ a | a<-xs , length a > length x]


--b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this
--list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first,
--others with a more frequent length come later.

lfsort [] = []
lfsort (x:xs) = lfsort [ a | a<-xs , freq a xs <= freq x (x:xs) ] ++  [x] ++ lfsort [ a | a<-xs , freq a xs > freq x (x:xs)]
                where freq a l = foldr (\x o -> if  length x == length a then (o+1) else o) 0 l
