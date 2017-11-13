import Control.Applicative
import Control.Monad

-- here PP Var is Var++ like C
data Exp = V Var | PP Var | Add Exp Exp | Div Exp Exp deriving (Show,Eq)
data Var = A | B | C deriving (Show,Eq)
data StateMonade s a = SM(s->(a,s))

state1 x | x == A = 4
         | x == B = 2
         | x == C = 0

eA = V A
eB = V B
eC = V C
eAdd= Add eA eB
eDiv= Div eA eB
ePPA= PP A
eDivByZero= Div eA eC


-- WITHOUT USING MONAD
--eval1:: Exp-> StateMonade (Var->a) a
eval1 (V a) = SM(\s->(s a,s))
eval1 (PP v) = SM (\s-> (s v, update s v ((s v)+1) ))
eval1 (Add e1 e2)= SM(\s-> let (SM f1)=eval1 e1
                               (v1,s1)=f1 s
                               (SM f2)=eval1 e2
                               (v2,s2)=f2 s1
                           in (v1+v2,s2))

eval1 (Div e1 e2) = SM(\s-> let (SM f1)=eval1 e1
                                (v1,s1)=f1 s
                                (SM f2)= eval1 e2 
                                (v2,s2)=f2 s1
                            in (div v1 v2,s2)) -- NOT HANDLING THE ERROR: DIVIDE BY ZERO

solve1 state exp = [("Final output",val),("A", s A),("B", s B),("C", s C)]
                   where (SM f)=eval1 exp 
                         (val,s)=f state 


-- USING MONAD

instance Functor (StateMonade s) where
         fmap f m = pure f <*> m 

instance Applicative (StateMonade s) where
         pure = return
         (<*>)  = ap

instance Monad (StateMonade s) where
         return a = SM (\s->(a,s))
         (>>=) (SM sf1) k = SM(\s-> let (v1,s1) = (sf1 s)
                                        (SM sf2)= k v1
                                    in sf2 s1)


eval2 (V v)  = SM (\s -> (s v,s))
eval2 (PP v) = SM (\s-> (s v, update s v ((s v)+1) ))
eval2 (Add e1 e2) = (eval2 e1) >>= (\i1 -> (eval2 e2) >>= (\i2-> return (i1+i2)) ) 
eval2 (Div e1 e2) = (eval2 e1) >>= (\i1->  (eval2 e2) >>= (\i2->return (div i1 i2)))
update s v val =(\x-> if v==x then val else s x)
    
solve2 state exp = [("Final output",val),("A", s A),("B", s B),("C", s C)]
                   where (SM f)=eval2 exp 
                         (val,s)=f state 

