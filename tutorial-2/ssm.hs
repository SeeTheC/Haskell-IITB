ssm l | l == [] = []
      | otherwise = ssm_aux (tail l) (head l)        

ssm_aux [] e = [e]
ssm_aux (x:xs) e | x <= e = (ssm_aux xs e)
                 | otherwise = if (length l1) < (length l2)
                               then l2
                               else l1
                               where l1 = e:(ssm_aux xs x)
                                     l2 = (ssm_aux xs e) 
