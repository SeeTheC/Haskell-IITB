-- https://wiki.haskell.org/99_questions/31_to_41
-- (**) Determine the prime factors of a given positive integer.
-- Construct a list containing the prime factors and their multiplicity.

prime_factors_mult n = reverse (prime_factors_mult_aux n []) 
prime_factors_mult_aux 1 rs = rs
prime_factors_mult_aux n rs = if null rs || (fst.head) rs /= i
                              then prime_factors_mult_aux (div n i) ((i,1):rs)
                              else prime_factors_mult_aux (div n i) ((i, (snd.head) rs + 1 ):tail rs)
                              where i = until (\x -> n `mod` x == 0) (+1) 2 


