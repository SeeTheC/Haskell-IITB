--(**) Rotate a list N places to the left.
--Hint: Use the predefined functions length and (++).

rotate l n = (drop x l) ++ (take x l)
             where x= mod ((length l) + n) (length l)


rotate1 l n = snd x ++ fst x
             where x= splitAt y l
                   y=mod ((length l) + n) (length l)

rotate2 l n | x ==0 = l
            | l == [] = []
            | otherwise= rotate2 ( (tail l) ++ [head l] ) (n-1)  
             where x= mod ((length l) + n) (length l)

