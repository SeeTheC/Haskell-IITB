filteredAccumulate s e operator f i r   | i == e = x
                                        | i < e  = filteredAccumulate s e operator f (i+1) x
                                        where x =  operator r (f s e i)

-- returns i if i is 13th multiple from start is odd else 0
filter0 s e i | mod (i - s + 1 ) 13 == 0 = i
              | otherwise = 0

-- returns i if i is odd else 0
filter1 s e i | odd i = i*i
              | otherwise = 0

-- returns i! if i is multiple of 3 else 1
filter2 s e i | mod i 3 == 0 = fact i
              | otherwise = 1
              where fact a | a==0 = 1 
                           | otherwise=a*(fact (a-1))

-- returns i^2 if i is prime else 0
filter3 s e i | (isPrime i 2) = i^2
              | otherwise = 0
              where isPrime n i | mod n i == 0 = False 
                                | i > x= True
                                | otherwise = isPrime n (i+1)
                                where x= floor (sqrt (fromInteger n) )

-- returns i if i is relative prime to e else 0
filter4 s e i | gcd i e == 1 = i
              | otherwise = 1


f0 s e =  filteredAccumulate s e (+) filter0 s 0
f1 s e =  filteredAccumulate s e (+) filter1 s 0
f2 s e =  filteredAccumulate s e (*) filter2 s 1
f3 s e =  filteredAccumulate s e (+) filter3 s 0
f4 s e =  filteredAccumulate s e (*) filter4 s 1

