-- (**) Replicate the elements of a list a given number of times.

repli [] _ =[];
repli (x:xs) n = (replicate n x)  ++ (repli xs n) 
