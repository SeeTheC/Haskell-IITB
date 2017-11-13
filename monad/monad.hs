--import Prelude hiding (return,(>>=))
data Exp = Con Int | Add Exp Exp | Div Exp Exp

e0=Con 0
e1=Con 1
e2=Con 2

-- Evaluatar With Error handling
eval1:: Exp -> Maybe Int
eval1 (Con a) = Just a 
eval1 (Add e1 e2) = case eval1 e1 of 
                        Nothing -> Nothing
                        Just i1 -> case eval1 e2 of 
                                        Nothing -> Nothing 
                                        Just i2 -> Just (i1 + i2)

eval1 (Div e1 e2) = case eval1 e1 of 
                        Nothing -> Nothing
                        Just i1 -> case eval1 e2 of 
                                        Nothing -> Nothing 
                                        Just 0 -> Nothing
                                        Just i2 -> Just (div i1 i2)

-- Type to be Monad it has to be Applicative funtor
--return:: a -> Maybe a
--return a = Just a
--(>>=):: Maybe a -> (a-> Maybe b) -> Maybe b
--(>>=) m f = case m of 
--                Nothing->Nothing
--                Just a -> f a

-- Error handling using Monad

eval2:: Exp -> Maybe Int
eval2 (Con a)=  return a
eval2 (Add exp1 exp2) = eval2 exp1 >>= f
                        where f x = eval2 exp2 >>= (\y-> return (x+y) )  

eval2 (Div exp1 exp2) = eval2 exp1 >>= f
                        where f x = eval2 exp2 >>= (\y-> if y== 0 then Nothing else return (x+y) )  




-- Error handling using Monad and do
eval3:: Exp -> Maybe Int
eval3 (Con a)=  return a
eval3 (Add e1 e2) = do
                    x<- eval3 e1
                    y<- eval3 e2
                    return (x+y)

eval3 (Div e1 e2) = do
                    x<- eval3 e1
                    y<- eval3 e2
                    if (y==0)
                    then Nothing
                    else return (x+y)
 
 


