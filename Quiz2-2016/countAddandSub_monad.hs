import Control.Monad.State
data Expr = I Var | Add Expr Expr | Sub Expr Expr | IF BExpr Expr Expr | PP Var deriving(Show,Eq)
data BExpr = Gt Expr Expr | Not BExpr deriving(Show,Eq)
data Var = X | Y | Z deriving(Show,Eq)

type ExpState=(Var->Int,Int,Int)


-- variable state
vState:: Var->Int
vState x | x == X = 4
         | x == Y = 2
         | x == Z = 0

state1::ExpState
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

--------------------------------------------

eval:: Expr-> State ExpState Int
eval (I v) = do 
             (f,ac,mc)<-get
             return (f v)



solve state exp = [("Final output",val),("Total Add",ta),("Total Sub",ts),("X", vs X),("Y", vs Y),("Z", vs Z)]
                   where (val,(vs,ta,ts))=(runState $ eval exp) state 


--solveBexp state exp = [("Final output",val)]
--                      where (val,(vs,ta,ts))=(runState $ evalBool exp) state 
