data CMaybe a = CNothing | CJust Int a deriving (Show)
instance Functor CMaybe where
        fmap f CNothing = CNothing
        fmap f (CJust counter x) = CJust (counter+1) (f x)




-- Tree
data Btree a = Node a (Btree a) (Btree a) | Null deriving Show

-- fmap will take one Box[a] (Box having some value) and function which take value and return another value. 
-- Fmap will first take the value say a from box apply function f on it  return new value let say b and pack the f output i.e into the Box i.e 
-- Box [b].

t1= Node 2 (Node 3 (Null) (Null)) (Node 4 (Null) (Null))
instance Functor (Btree) where
    fmap f (Null) = Null
    fmap f (Node a lt rt) = Node b (fmap f lt) (fmap f rt)
                            where b=f a



