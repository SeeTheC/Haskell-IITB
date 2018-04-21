data Tree a b = Node a (Tree a b) (Tree a b) | Leaf b deriving (Show)

tree1=  Node 'a' (Node 'b' (Leaf 4) (Leaf 6)) (Leaf 7)  

instance Functor (Tree a) where
        fmap f (Leaf b) = Leaf (f b)
        fmap f (Node a lt rt) = (Node a (fmap f lt) (fmap f rt)) 

sqTree= fmap (\x->x*x) tree1
