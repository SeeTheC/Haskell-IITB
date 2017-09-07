-- The following iterative sequence is defined for the set of positive integers:
-- n → n/2, if n is even
-- n → 3n + 1, if n is odd and 6= 1
-- 1, otherwise
-- Example:
-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

-- Write a function longestchain n which finds the length of the longest chain
-- starting from any number less than or equal to n.

longestchain n | n == 0 = 0
               | otherwise= max (chainLength n) (longestchain (n-1)) 

chainLength n | n == 1 = 1
              | n `mod` 2 ==0 =  1 +   chainLength (div n 2)
              | n `mod` 2 == 1 = 1 +  chainLength (3*n+1)

-- tr: tail recursion


longestchain_tr n r | n == 1 = max 1 r
                    | otherwise= longestchain_tr (n-1) (max (chainLength_tr n 1) r)


-- tr: tail recursion
chainLength_tr n r | n == 1 = r
                   | n `mod` 2 ==0  =  chainLength_tr (div n 2) (r+1)
                   | n `mod` 2 == 1 =  chainLength_tr (3*n+1) (r+1)

	

cp [] =[[]]
cp (x:xs) = []

