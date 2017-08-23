bridge l1 l2 = [bridge_aux l1 l2] ++ [bridge_aux l2 l1] 
bridge_aux [] _ = []
bridge_aux _ [] = []
bridge_aux (x:l1s) l2 = if length xTaken > length xNotTaken then xTaken else xNotTaken
                        where xTaken=if (length.find) l2 /= length l2 then x:bridge_aux l1s (find l2) else bridge_aux l1s l2  
                              xNotTaken=bridge_aux l1s l2
                              find=reverse.takeWhile (\i-> i /=x).reverse
