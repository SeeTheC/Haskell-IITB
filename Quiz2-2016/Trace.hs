data Exp = Con Int | Add Exp Exp | Div Exp Exp deriving (Show,Eq)
type Trace = String
type  ErrorMesssage = String

data Result a = Normal a | Error ErrorMesssage deriving (Show,Eq)

instance Functor Result where
         fmap f (Normal a) = Normal (f a)
         fmap f (Error em)= Error em

instance Applicative Result where
         pure i = Normal i 
         (<*>) (Normal f) mv = f <$> mv
 
instance Monad Result where
      return i = Normal i 
      (>>=) (Normal i)  f = (f i)
      (>>=) (Error em)  f = Error em


eval:: Exp->Result (Trace, Int)
eval e@(Con i) = Normal ("Value of "++(show e)++" is "++ (show i) ++ ",", i)

eval e@(Add e1 e2) = do
                     (tm1,a)<-eval e1
                     (tm2,b)<-eval e2
                     return (tm1 ++ "\n" ++ tm2 ++ "\nValue of "++(show e)++" is "++ (show (a+b)) ++ ",", a+b)


eval e@(Div e1 e2) = do
                     (tm1,a)<-eval e1
                     (tm2,b)<-eval e2
                     if b == 0 
                     then Error (tm1 ++ "\n" ++ tm2 ++ "\nERROR: In exp \""++ (show e) ++ "\" Divide By Zero Error")
                     else return (tm1 ++ "\n" ++ tm2 ++ "\nValue of "++(show e)++" is "++ (show (div a b)) ++ ",", div a b)



exp1 = (Con 2)
exp2 = Add (Add (Con 2) (Con 0)) (Con 1)
exp3 = Add (Div (Con 2) (Con 0)) (Con 1)
main = case (eval exp3) of
            Normal (s,a) ->putStrLn s
            Error em -> putStrLn em 
