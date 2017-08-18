g = [(1,2),(2,3),(2,8),(8,10),(10,3),(8,9),(1,10),(1,9)]
g1 = [(1,2)]
g2 = [(1,2),(2,3),(2,4)]
g3 = [(1,2),(2,3),(2,8),(8,10)]
g4 = [(1,2),(2,3),(3,4),(4,1)]


-- lv: loop variable like i in for loop
makepath n g = makepath_aux n g [n] g
makepath_aux _ _ _ []  = []
makepath_aux n g p ((x,y):lv) =   if (n==x) && ( not (elem y p) )
                                  then (path x y) ++ (makepath_aux n g p lv)                               
                                  else if (n==y) && ( not (elem x p) )
                                  then (path y x) ++ (makepath_aux n g p lv)                                                        
                                  else  makepath_aux n g p lv
                                  where path a b = if (length (subpath b) ) == 0
                                                   then [[a,b]]
                                                   else addElement n (subpath b)
                                        subpath a = (makepath_aux a g (a:p) g)
                                        -- add element in list of nested list 
                                        addElement e l = map (e:) l  


getAllVertices g = foldl f id g
                   where id=[]
                         f a (x,y) = a ++ (exist x a) ++ (exist y a) 
                         exist v l = if not (elem v l) then [v] else []


-- assuming path is longest path start with the intial node
is_hc path g | (length path) == (length vertices) =  if elem e1 g || elem e2 g then True else False                                                   
             | otherwise = False
               where a=head path
                     b=path!! ( (length path) - 1)
                     e1=(a,b)
                     e2=(b,a)
                     vertices=(getAllVertices g)

hc g  = [ x | x<- allPath, is_hc x g ]
        where allPath= concat [ makepath n g | n<-(getAllVertices g) ]





