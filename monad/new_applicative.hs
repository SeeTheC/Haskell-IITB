class NewApplicative m where
    (<**>) :: (Functor n) => m (a -> b) -> n a -> n b


instance NewApplicative Maybe where
    (Just x) <**> box = x <$> box

instance NewApplicative [] where
    [x] <**> box = x <$> box
    -- Note: I have no idea how to define it for [] or [x,y..]

data Tree a = Leaf a | Node [Tree a] deriving (Show)
instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node ts) = Node (map (fmap f) ts)


example = Just (+) <**> [1] <**> Node [Node [Leaf 2], Leaf 4]
