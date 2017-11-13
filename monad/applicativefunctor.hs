-- Tree
data Btree a = Node a (Btree a) (Btree a) | Null deriving Show

-- fmap will take one Box[a] (Box having some value) and function which take value and return another value. 
-- Fmap will first take the value say a from box apply function f on it  return new value let say b and pack the f output i.e into the Box i.e 
-- Box [b].
instance Functor (Btree) where
    fmap f (Null) = Null
    fmap f (Node a lt rt) = Node b (fmap f lt) (fmap f rt)
                            where b=f a


-- For "Type" to be Applicative functor  it show should be first instance of Applicative Functor
-- <*> will take one Box[a] (Box having some value) and function which take value and return another value. BUT THAT FUNCTION IS ALSO INSIDE THE BOX. Note: here Box means "type"
-- <*> will first take the value say a and function from box apply function f on it, which return new value let say b and it then pack back that value i.e b in Box [b].
instance Applicative Btree where
   pure a  = Node a Null Null
   (<*>)  _ Null = Null
   (<*>)  Null _ = Null
   (<*>) (Node f lft rft) (Node a lt rt) = Node (f a) (lft <*> lt) (lft <*> rt) 


t1=  Node 1 (Node 2 (Null) (Null)) (Node 3 (Null) (Null))
ft1 = Node (\x->x*2) (Node (\x->x*x*x) (Null) (Null)) (Node (\x->x*x*x) (Null) (Null))

-- terminal$ ft1 <*> t1
