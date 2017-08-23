fewest_moves l= (length.fewest_moves_aux) l
fewest_moves_aux [] = []
fewest_moves_aux l  = if head l == 1 
                      then 
		          if length pw1 <  length pw2 then pw1 else pw2
                      else  
		          if length pb1 <  length pb4 then pb1 else pb4
                      where pw1 = head l: (fewest_moves_aux.drop 1) l
                            pw2 = head l: (fewest_moves_aux.drop 2) l
		            pb1 = head l: (fewest_moves_aux.drop 1) l
                            pb4 = head l: (fewest_moves_aux.drop 4) l  

