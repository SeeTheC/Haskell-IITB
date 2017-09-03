-- (*) Find the K'th element of a list. The first element in the list is number 1
elementAt l k = fst (foldl (\(o,i) x-> if (i+1) == k then (x,i+1) else (o,i+1) ) (err1,0) l)
                where err1 = error "No such element exist Found" 
