import Control.Monad.State
data Exp = V Var | PP Var | Add Exp Exp | Div Exp Exp deriving (Show,Eq)
data Var = A | B | C deriving (Show,Eq)

type ExpState= Var->Int
exp1 = Add (PP A) (V B)
exp2 = Div (V B) (V A)
exp3 = Div (V B) (V C)
exp4 = Add (V A) (V B)

state1 a | a == A = 2
         | a == B = 4
         | a == C = 0

update state var val = (\v-> if v == var then val else state v)

eval:: Exp-> State ExpState Int
eval (V v)= do
            s<-get
            return (s v)

eval (PP v) = do
              s<-get
              put (update s v ((s v) + 1))
              return (s v)

solve exp state= ["output:"++(show o), "A:"++ (show (rs A)),"C:"++ (show (rs B)),"C:"++ (show (rs C))]
                  where (o,rs)=(runState $ eval exp) state 

