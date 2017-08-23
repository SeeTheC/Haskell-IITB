append l1 l2 = foldr f id l1
               where id=l2
                     f x prvOut = x:prvOut	
