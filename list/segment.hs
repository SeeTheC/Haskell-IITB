import Data.List
segment [] =[]
segment l = tail (inits l) ++ segment (tail l)
