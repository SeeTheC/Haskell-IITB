--Insert an element at a given position into a list.

insertAt c l pos = fst x ++ [c] ++ snd x
                   where x= splitAt (pos-1) l 
