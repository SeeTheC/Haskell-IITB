--(*) Split a list into two parts; the length of the first part is given.
--Do not use any predefined predicates.
split l n | n==0 = ([],l)
          | l == [] = ([],[])
          | otherwise=( head l:fst subs, snd subs)
          where subs= split (tail l) (n-1)  
