type Graph = [(Int,Int)]
type Node = Int
type Path = [Int]

g  = [(1,2),(2,3),(3,10),(2,8),(8,10),(10,8),(10,3),(8,9),(1,10),(1,9),(9,1),(10,1)]::Graph
g1 = [(1,2)]::Graph
g2 = [(1,2),(2,3),(2,4)]::Graph
g3 = [(1,2),(2,3),(2,8),(8,10)]::Graph
g4 = [(1,2),(2,3),(3,4),(4,1)]::Graph
g5 = [(1,2),(2,3),(3,4)]::Graph
g6 = [(1,2),(1,3)]::Graph


-- First way of writing these
-- lv: loop variable like i in for loop
makepath1 n g = makepath1_aux n g [n] g
makepath1_aux _ _ _ []  = []
makepath1_aux n g p ((x,y):lv) =   if (n==x) && ( not (elem y p) )
                                  then (path x y) ++ (makepath1_aux n g p lv)                               
                                  else  makepath1_aux n g p lv
                                  where path a b = if (length (subpath b) ) == 0
                                                   then [[a,b]]
                                                   else addElement n (subpath b)
                                        subpath a = (makepath1_aux a g (a:p) g)
                                        -- add element in list of nested list 
                                        addElement e l = map (e:) l  

--- 2nd way of writing these
makepath:: Node->Graph->[Path] 
makepath n g = makepath_aux n g [n]
makepath_aux n g p = [ n:sp  |  (x,y) <- g, sp<-(guardList x y)]
            where emptyCheck x l= if (l==[]) then [[x]] else l
                  guardList a b = if (a==n) && not (elem b p) 
                                  then emptyCheck b (makepath_aux b g (b:p))
                                  else [] 


getAllVertices g = foldl f id g
                   where id=[]
                         f a (x,y) = a ++ (exist x a) ++ (exist y a) 
                         exist v l = if not (elem v l) then [v] else []

is_hc:: Path-> Graph->Bool 
-- assuming path is longest path start with the intial node
is_hc path g | (length path) == (length vertices) =  if elem (path!! ( (length path) - 1),head path) g then True else False                                                   
             | otherwise = False
               where vertices=(getAllVertices g)

hc:: Graph->[Path] 
hc g  = [ x | x<- allPath, is_hc x g ]
        where allPath= concat [ makepath n g | n<-(getAllVertices g) ]





