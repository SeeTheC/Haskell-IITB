data Peano = Zero | Succ Peano deriving (Show,Eq)

-- Test: intToPeano 5 o/p Succ (Succ (Succ (Succ (Succ Zero))))
intToPeano n | n==0 = Zero
             | n < 0 = error "Negative Peano Number Not define"
	     | otherwise = Succ (intToPeano (n-1) )


-- Test: peanoToInt (Succ (Succ (Succ (Succ (Succ Zero))))) o/p 5
peanoToInt Zero = 0
peanoToInt (Succ n) | n==Zero = 1
                    | otherwise = 1+(peanoToInt n) 

instance Num Peano where
                   x + y =  intToPeano (a+b)
                         where a=peanoToInt(x)
                               b=peanoToInt(y)

                   x * y =  intToPeano (a*b)
                         where a=peanoToInt(x)
                               b=peanoToInt(y)
 
                   x - y =  intToPeano (a-b)
                         where a=peanoToInt(x)
                               b=peanoToInt(y)

                   signum x | x==Zero = Zero
                            | otherwise = Succ Zero                            

                   abs x = x
