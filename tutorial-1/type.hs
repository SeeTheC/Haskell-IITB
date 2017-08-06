-- Defination of Set in terms of membership condition 
type Set a = a -> Bool

emptySet:: t->Bool
emptySet a = False

-- Insert into set
insert:: (Eq a) => Set a->a->Set a 
insert s a = (\x -> (s x) || (x==a) )

member:: Set a -> a -> Bool
member s a = (s a)

union:: Set a -> Set a -> Set a
union s1 s2 = (\x -> s1 x || s2 x)

intersection:: Set a -> Set a -> Set a
intersection s1 s2 = (\x -> s1 x && s2 x)

difference:: Set a -> Set a -> Set a
difference s1 s2 = (\x-> s1 x && not(s2 x) )

