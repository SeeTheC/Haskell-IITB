-- https://wiki.haskell.org/99_questions/31_to_41
-- primeFactors
--(**) Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.
--  >primeFactors 315
--  >[3, 3, 5, 7]

primeFactors 1 = []
primeFactors n = [i] ++ (primeFactors (div n i)) 
                 where i = until (\x-> n `mod` x ==0) (+1) 2
                       
