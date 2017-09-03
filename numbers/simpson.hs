simpson f a b n = (h/3) * sumseries (\k-> c k + y k) n
                  where h =(b-a)/ fromInteger n
                        c k | k == 0 =  1
                            | k == n =  1
                            | even k =  2
                            | odd  k =  4
                        y k = f (a + fromInteger k*h)
                        sumseries f1 n | n < 0 = 0
                                       | otherwise= f1 n + sumseries f1 (n-1)
