hof a b c [] = b
hof a b c (x:xs) = c (a x) (hof a b c xs)

map1 f l=hof f [] (:) l
foldr2 f i l= hof id i f l 
reverse1 l = hof id [] (\x y-> y ++ [x]) l
takeWhile1 f l = hof (\x-> (f x)) []

