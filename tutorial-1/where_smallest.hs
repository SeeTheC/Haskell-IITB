where_smallest f a b | a == b = a
                     | otherwise = if t1val < t2val 
                                   then a
                                   else t2
			           where t1val = f a
                   			 t2 = where_smallest f (a+1) b
                                         t2val= f t2

f1 x = x*x
f2 x = (x-2)*(x-10)
