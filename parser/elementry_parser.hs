import Prelude hiding ( (<*>))

type Parser symbol result = [symbol] -> [([symbol],result)]
infixr 5 <*>
infixr 5 <@
infixr 4 <|>

-- parser that will just identify 'a'
symbola :: Parser Char Char
symbola [] = []
symbola (x:xs) = [ (xs,'a') | x=='a']



-- parser that will just identify any element

symbol:: (Eq s) => s->Parser s s
symbol k []= []
symbol k (x:xs) = [ (xs,x) | k==x]


-- satisfy 
satisfy :: (s->Bool) -> Parser s s
satisfy p [] = []
satisfy p (x:xs) = [(xs,x) | p x]

-- example Abstract way
symbol' k l = satisfy (k==) l

(<*>) :: Parser s a -> Parser s b -> Parser s (a,b)
(p1 <*> p2 ) xs  = [ (xs2,(v1,v2))  | (xs1,v1) <-p1 xs,(xs2,v2)<-p2 xs1]

(<|>) :: Parser s a -> Parser s a -> Parser s a
(p1 <|> p2 ) xs  = p1 xs ++ p2 xs


-- trasform

(<@) :: Parser s a -> (a -> b) -> Parser s b 
(p <@ f) xs = [ (ys, f v)   |  (ys,v)<-p xs]

p1 xs = (symbol 'a' <*> symbol 'b' <*> symbol 'c') xs
