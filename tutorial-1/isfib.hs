isfib k = loop_1 fib 1 k

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

loop f i n | i > n = False
           | n == f(i) = True
           | n < f(i) = False
           | otherwise = loop f (i+1) n

-- Using variable
loop_1 f i n | i > n = False
             | n == x = True
             | n < x = False
             | otherwise = loop_1 f (i+1) n
             where x=f(i)
