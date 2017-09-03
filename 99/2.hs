-- (*) Find the last but one element of a list.
myButLast [] = error "Empty"
myButLast l | length l == 1 = error "Only element"
            | (tail.tail) l == [] =  head l
            | otherwise = (myButLast.tail) l
