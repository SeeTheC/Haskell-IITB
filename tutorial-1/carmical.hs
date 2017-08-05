carmical n = solve n 1

solve 0 i = i-1
solve n i = if (iscarmical i 1 0)
	    then  solve (n-1) (i+1)
	    else  solve (n) (i+1)

-- loop from 1 to k to find the check the mod of all co-prime of k. Also simulateously it checks the whether k in prime or not  
-- By "Fermat little Theoram" a^(p-1) mod p = 1 for every PRIME P where 1<=a<=p-1
-- But we want check for carmical number. These are NON-PRIME number. Which satisfy the equation a^(p-1) mod p where 1<=a<=p-1 and is 'a' COPRIME w.r.t p where 'p' is NON PRIME.
  
iscarmical k i n	| i == k =   if n == k-1 -- means k is prime
				     then False
				     else True
			| (gcd i k) == 1 = 	if (i^(k-1) `mod` k) == 1
					   	then iscarmical k (i+1) (n+1)
						else False
			| otherwise = iscarmical k (i+1) n
