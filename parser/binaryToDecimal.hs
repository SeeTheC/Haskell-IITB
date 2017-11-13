type Parser symbol result= [symbol]->[([symbol],result)]
data Tree = Nil | Bin (Tree,Tree) deriving (Eq,Show)

symbol:: (Eq a)=>a->Parser a a
symbol a [] = []
symbol a (x:xs) = [(xs,a) | x==a]

epsilon:: Parser s ()
epsilon xs = [ (xs,())]

infixr 6 <#>
infixr 5 <@
infixr 4 <|>

-- sequntional i.e AND
(<#>) :: Parser s a-> Parser s b-> Parser s (a,b)
(p1 <#> p2) xs = [ (rs2, (a,b))   |  (rs1,a)<-p1 xs, (rs2,b)<-p2 rs1 ]

-- OR
(<|>) :: Parser s a-> Parser s a-> Parser s a
(p1 <|> p2) xs = (p1 xs) ++ (p2 xs) 

-- function applied to result of parser
(<@):: Parser s a->(a->b)->Parser s b
(p1 <@ f) xs= [ (rs,f r) | (rs,r)<-p1 xs]

--fltering the list of successes for null rest strings
just:: Parser s a -> Parser s a
just p = filter (null.fst) . p

-- Test parser
parser1 = symbol 'a' <#> symbol 'b'  <#> symbol 'c'
 
-- binToDecParser o/p (decimal number, length of binary string)

binToDecParser:: Parser Char (Int,Int)
binToDecParser = (symbol '1' <#> binToDecParser ) <@ (\(c,(d,l))->(d+(2^(l)),l+1))
                 <|> (symbol '0' <#> binToDecParser ) <@ (\(c,(d,l))->(d,l+1))
                 <|> epsilon <@ (\a->(0,0))

binToDec s = fst $ snd $ head $ (just binToDecParser) s

