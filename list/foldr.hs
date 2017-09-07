import Prelude hiding ((++),length,dropWhile)

length l = (foldr (\x o -> o + 1) 0) l

l1 ++ l2  = foldr (\x o-> x:o) l2 l1

dropWhile p l = foldr f ([],[]) l
                where f x o = if p x then (fst o, x:snd o)
                              else (x:snd o, x:snd o) 


inits l =  foldr (\x o-> []:map (x:) o) [[]] l

tails l =  foldr (\x o-> (x:head o):o) [[]] l
