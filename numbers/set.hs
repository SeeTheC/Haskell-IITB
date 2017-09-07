data Set a = Set (a->Bool)

emptyset= Set (\x->False)

member (Set s) x = s x

insert (Set s) a = Set (\x -> s x || x==a)

union (Set s1) (Set s2) = Set (\x -> s1 x || s2 x)

intersect (Set s1) (Set s2) = Set (\x -> s1 x && s2 x)

diff (Set s1) (Set s2) = Set (\x -> s1 x && not (s2 x) ) 
