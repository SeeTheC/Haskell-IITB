-- for oveloading same name
import Prelude hiding (lookup)

-- a
rotate n l =  snd t  ++ fst t
              where t = splitAt x l
                    x = (n + (length l) ) `mod` (length l)

-- b
alpha="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
makeKey n = zip alpha (rotate n alpha)


-- c
lookup :: Char-> [(Char,Char)] -> Char
lookup c l = if null r then c else snd (r!!0)
             where r = filter ( \(a,b) -> a == c ) l 

-- d
encipher n c = lookup c (makeKey n)

-- e
-- Using asci
normalize s = [ if (fromEnum x)>=97 &&  (fromEnum x)<(97+26) then  toEnum ((fromEnum x) - 32)::Char else x | x<-s]
lApha = "abcdefghijklmnopqrst"

