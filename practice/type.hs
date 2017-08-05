type Point = (Float,Float)
type Region = Point -> Bool 
circleMaker r = \(x, y) -> x ^ 2 + y ^ 2 <= r ^ 2
rectangleMaker :: Float -> Float -> Region
rectangleMaker l b = \(x,y) -> (abs x) <= l/2 && (abs y) <= b/2

type Set a = a -> Bool
s1:: (Num a)=>Set a
s1 a = False

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

