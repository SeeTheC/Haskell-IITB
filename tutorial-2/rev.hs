rev l1 = foldr f id l1
         where id =[]
               f x prvOut=prvOut++[x]
