data Btree a = Leaf a | Fork (Btree a) (Btree a) deriving (Show,Eq)

t = Fork (Leaf 'a') (Leaf 'b')
--  Fork () ()
t1 =  Fork (Leaf 'a') ( Fork (Fork (Leaf 'b') (Fork (Leaf 'c') (Leaf 'd'))) (Fork (Fork ( Leaf 'e') (Leaf 'f')) (Fork (Leaf 'g') (Leaf 'h'))))


-- returns the leaf the remaining unparsed list
traverseTree (Leaf a) l = (a,l)
traverseTree (Fork lt rt ) l | l == [] =   error "No value exists for the given path."  
                             | head l == 0 =   traverseTree lt (tail l)
                             | head l == 1 =   traverseTree rt (tail l)

-- decode
decode:: [Int]-> Btree Char-> [Char]
decode [] t = []
decode l  t =  [char] ++ decode sublst t
               where (char,sublst)=traverseTree t l

-- search value in tree and returns its path of traversal
search:: Char -> Btree Char -> [Int]
search c (Leaf a) = [] -- not found
search c (Fork lt rt) | lt == (Leaf c) = [0]
                      | rt == (Leaf c) = [1]
                      | otherwise = r
                        where lst = search c lt
                              rst = search c rt
                              r = if length rst > 0 then 1:rst 
                                  else if length lst > 0 then 0:lst
                                  else []


-- encode sttriing
encode:: [Char] -> Btree Char -> [Int]
encode l t = concat $ [ check (search c t) |   c<-l ]
             where check l = if (not.null) l then l else error "Wrong char i.e encode doesnot exist for some char"
