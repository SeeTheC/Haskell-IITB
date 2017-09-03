-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.

slice l i j= fst (splitAt (j-i+1) (snd (splitAt (i-1) l) ) )


slice1 l i j | l == [] = []
             | i == 0 && j ==0 = []
             | i == 1 && j>=1 = (head l):(slice (tail l) i (j-1))
             | otherwise= slice (tail l) (i-1) (j-1)
