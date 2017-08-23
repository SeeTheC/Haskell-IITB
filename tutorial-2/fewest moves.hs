fewest_moves [] = []
fewest_moves (x:ls)  = pw1
                       where pw1 = fewest_moves ls
                             pw2 = (fewest_moves.drop) ls  
