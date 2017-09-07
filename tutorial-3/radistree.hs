data Color = Black | White deriving (Show,Eq)
data RadixTree = Leaf Color | Node Color RadixTree RadixTree deriving (Show,Eq)
type BinaryString = [Integer]
--Node Black () ()

t0=Leaf White
t1=Node White (Node Black (Node White (Leaf Black) (Leaf White)) (Leaf Black)) (Node White (Leaf Black) (Leaf Black))

-- if list ends at White color either leaf or Node
elementExist [] (Leaf c) | c == White = True
                         | otherwise= False
elementExist [] (Node c lt rt) | c == White = True
                               | otherwise= False
elementExist xs (Leaf c) = False
elementExist (a:xs) (Node c lt rt) | a == 1  = elementExist xs rt
                                   | a == 0  = elementExist xs lt

-- insert
insert [] (Leaf c) = Leaf White
insert [] (Node c lt rt) = Node White lt rt
insert (a:xs) (Leaf c) | a == 0 = Node c (insert xs (Leaf Black) ) (Leaf Black)
                       | a == 1 = Node c  (Leaf Black) (insert xs (Leaf Black))
insert (a:xs) (Node c lt rt) | a == 0 = Node c (insert xs lt) rt
                             | a == 1 = Node c lt (insert xs rt)


