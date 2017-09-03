-- Create a list containing all integers within a given range.

range a b | a > b = []
          | otherwise=a:[ x+1 | x<-range a (b-1)]


range1 a b = [a..b]

range2 a b= take (b-a+1) $ iterate (+1) a

range3 a b= enumFromTo a b
