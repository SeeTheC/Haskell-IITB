-- maximum segment size
import Data.List
segment [] =[]
segment l = tail (inits l) ++ segment (tail l)

mss l = (maximum.map (sum).segment) l
