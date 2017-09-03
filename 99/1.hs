-- https://wiki.haskell.org/99_questions/1_to_10
--(*) Find the last element of a list.
myLast:: (Foldable t) => t a ->a
myLast l  = foldr1 (\x o->o) l
myLast1 l = foldr1 (flip const) l 
