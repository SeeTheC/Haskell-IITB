import Control.Applicative
import Control.Monad

-- here PP Var is Var++ like C
data Exp = V Var | PP Var | Add Exp Exp | Div Exp Exp deriving (Show,Eq)
data Var = A | B | C deriving (Show,Eq)
data StateMonade s a = SM(s->(a,s))
type State=Var->Maybe Integer 

instance Functor (StateMonade s) where
         fmap f a = pure f <*> a
instance Applicative (StateMonade s) where
         pure = return
         (<*>)= ap
instance Monad (StateMonade s) where
         return a  = SM (\s->(a,s))
         (>>=) m k = SM(\s-> let (SM sf)= m
                                 (v1,s1)= sf s
                                 (SM sf2)= k v1
                             in sf2 s1)
----------------------------------------------------
state1::State
state1 x | x == A = return 4
         | x == B = return 2
         | x == C = return 0

eA = V A
eB = V B
eC = V C
eAdd= Add eA eB
eDiv= Div eA eB
ePPA= PP A
eDivByZero= Div eA eC

update s v val = (\x->if x==v then val else s v)

eval:: Exp -> StateMonade State (Maybe Integer)
eval (V v)  =SM(\s->(s v,s))
eval (PP v) =SM(\s->(s v,update s v ( pure (+1) <*> (s v) ) ) )
eval (Add e1 e2)= do
                  v1<-(eval e1)
                  v2<-(eval e2)
                  return (v1 >>= (\a->pure (+a) <*> v2)) -- Adding two may be value: Just (3) >>= (\a-> pure (+a) <*> Just 4)
                  -- In return >>= (then) operator is of maybe not StateMonade

eval (Div e1 e2)= do
                  v1<-(eval e1)
                  v2<-(eval e2)
                  if v2 == pure 0 -- Just 0 
                  then return Nothing 
                  else return (v1 >>= (\a->pure (div a) <*> v2))

solve state exp = [("Final output",val),("A", s A),("B", s B),("C", s C)]
                   where (SM sf)=eval exp 
                         (val,s)=sf state 

