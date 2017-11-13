import Control.Applicative 
import Control.Monad

data StateMonad s a = SM (s->(a,s))

instance Functor (StateMonad s) where
         fmap f a = pure f <*> a

instance Applicative (StateMonad s) where
         pure = return
         (<*>) = ap


---------------------
instance Monad (StateMonad s) where
          return a =SM(\s->(a,s))
          (>>=) (SM sf1) f = SM(\s -> let (v1,s1) = sf1 s
                                          (SM sf2) = f v1
                                      in sf2 s1)


data Expr = I Var | Add Expr Expr | Sub Expr Expr | IF BExpr Expr Expr| PP Var deriving(Show,Eq)
data BExpr = Gt Expr Expr | Not BExpr deriving(Show,Eq)
data Var = X | Y | Z deriving(Show,Eq)
type State = (Var -> Int, Int, Int) -- see the type very care fully, its tuble three value one Function and other two are Int

-- variable state
vState:: Var->Int
vState x | x == X = 4
         | x == Y = 2
         | x == Z = 0

state1::State
state1 = (vState,0,0)

eX = I X
eY = I Y
eZ = I Z
eAdd= Add eX eY
eSub= Sub eX eY
ePPX= PP X
eGt= Gt eX eY
eNot= Not eGt
eIf= IF eGt eAdd eSub
eBig = IF (Not (Gt (PP X) eY )) eAdd eIf 
---------------------------------------------------------------------
incAdd::State->State
incAdd (vs,ta,ts) = (vs,ta+1,ts)
incSub::State->State
incSub (vs,ta,ts) = (vs,ta,ts+1)
update vs v val = (\x-> if x==v then val else vs x)  
-- ta: total addition and total substraction
eval:: Expr-> StateMonad State Int
eval (I v)  = SM(\s@(vs,ta,ts)->(vs v,s))           
eval (PP v) = SM(\s@(vs,ta,ts)->(vs v,us s))
              where us (vs,ta,ts) = (update vs v ((vs v) + 1),ta+1,ts)

eval (Add e1 e2)= (eval e1) >>= (\v1-> (eval e2) >>= (\v2-> return (v1+v2) >>= us )  )  
                  where us v =SM(\s->(v,incAdd s))                    

eval (Sub e1 e2)= do
                  v1<- eval e1
                  v2<- eval e2
                  v3<- return (v1-v2)
                  us v3
                  where us v =SM(\s->(v,incSub s))

eval (IF be e1 e2) = (evalBool be) >>=(\b-> if (b) then (eval e1) else (eval e2) )



evalBool:: BExpr->StateMonad State Bool
evalBool (Gt e1 e2) = do
                      v1<-(eval e1)
                      v2<-(eval e2)
                      return (v1>v2)
evalBool (Not e1) = do
                    v1<-evalBool e1
                    return (not v1)  

solve state exp = [("Final output",val),("Total Add",ta),("Total Sub",ts),("X", vs X),("Y", vs Y),("Z", vs Z)]
                   where (SM f)=eval exp 
                         (val,(vs,ta,ts))=f state 


solveBexp state exp = [("Final output",val)]
                      where (SM f)=evalBool exp 
                            (val,(vs,ta,ts))=f state 
