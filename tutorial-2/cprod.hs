cprod [] = [[]]
cprod (x:y:ys) = cprod_aux nl ys                 
                 where nl=[ a : [b] | a<-x, b<-y ]

cprod_aux x []     = x
cprod_aux x (y:ys) = cprod_aux nl ys
                     where nl= [ a ++ [b] | a<-x, b<-y ]
