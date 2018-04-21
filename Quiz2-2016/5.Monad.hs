data Tree a b = Node a (Tree a b) (Tree a b) | Leaf b deriving (Show,Eq)

tree1 = Node 1 (Leaf (*1) ) (Node 2 (Leaf (*2) ) (Leaf (*3) ))
tree2 = Node 10 (Leaf 4)  (Node 20 (Leaf 5) (Leaf 6))

instance Functor (Tree a) where
         fmap f (Leaf b) = Leaf (f b)
         fmap f (Node a lt rt) = Node a (fmap f lt) (fmap f rt)

instance Applicative (Tree a) where 
         pure b  = Leaf b
         (<*>) (Leaf f) mv = f <$> mv
         (<*>) (Node a lt rt) mv = Node a (lt <*> mv) (rt <*> mv)   


instance Monad (Tree a) where 
         return b = Leaf b
         (>>=) (Leaf b) fm = fm b
         (>>=) (Node a lt rt) fm = Node a (lt >>= fm) (rt >>= fm)


foo = (Leaf 3) >>= (\a-> Leaf a)
foo1 t  = do
         x <- t
         return (x*2)

solve = do
        x<-Node 10 (Leaf 4)  (Node 20 (Leaf 5) (Leaf 6))
        Node 1 (Leaf (x*1) ) (Node 2 (Leaf (x*2) ) (Leaf (x*3) ))
