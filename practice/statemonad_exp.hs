data Exp = V Var | PP Var | Add Exp Exp | Div Exp Exp deriving (Show,Eq)
data Var = A | B | C deriving (Show,Eq)

type State= Var->Int
exp1 = Add (PP A) (V B)
exp2 = Div (V B) (V A)
exp3 = Div (V B) (V C)
exp4 = Add (V A) (V B)

state1 a | a == A = 2
         | a == B = 4
         | a == C = 0


update state var val = (\v-> if v == var then val else state v)
eval1:: Exp->(State->(Int,State))
eval1 (V a)=(\s -> (s a,s)) 
eval1 (PP a)= (\s-> (s a, update s a ((s a) + 1) )  )
eval1 (Add e1 e2)=(\s -> let 
                            (a,s1)= eval1 e1 s
                            (b,s2)= eval1 e2 s1
                         in (a+b,s2)
                  ) 

eval1 (Div e1 e2)=(\s -> let 
                             (a,s1)=eval1 e1 s
                             (b,s2)=eval1 e2 s1
                          in (div a b, s2) 
                  )

solve1 exp state= ["output:"++(show o), "A:"++ (show (rs A)),"C:"++ (show (rs B)),"C:"++ (show (rs C))]
                  where (o,rs)=eval1 exp state


----------------------------------------------------------------------------
data StateMonad a = SM (State->(a,State))

instance Functor StateMonad where
         fmap f (SM vf)=SM (\s->(f $ fst (vf s),s))

instance Applicative StateMonad where
         pure a=SM (\s->(a,s))
         (<*>) (SM vf) mv = SM (\s-> let (f,s1)= vf s
                                         (SM evf)= f <$> mv
                                      in evf s1
                               )

instance Monad StateMonad where
         return a = SM (\s -> (a,s))
         (>>=) (SM vf) fm=SM(\s->let 
                                   (a,s1) = vf s
                                   (SM vf1)= fm a
                                 in vf1 s1
                            )       


eval2:: Exp->StateMonad Int
eval2 (V a)=SM (\s -> (s a,s))
eval2 (Add e1 e2)= (eval2 e1) >>= (\a-> (eval2 e2) >>= (\b-> return (a + b) ))
eval2 (Div e1 e2)= (eval2 e1) >>= (\a-> (eval2 e2) >>= (\b-> return (div a b) ))

solve2 exp state= ["output:"++(show o), "A:"++ (show (rs A)),"C:"++ (show (rs B)),"C:"++ (show (rs C))]
                  where (SM vf)= eval2 exp
                        (o,rs)=vf state 

-----------------------------------------------------


eval3:: Exp->StateMonad Int
eval3 (V a)=SM (\s -> (s a,s))
eval3 (Add e1 e2)= do 
                   a <- (eval3 e1)
                   b <- (eval3 e2)
                   return (a + b)  
eval3 (Div e1 e2)= do
                   a <- (eval3 e1)
                   b <- (eval3 e2)
                   return (div a b)


solve3 exp state= ["output:"++(show o), "A:"++ (show (rs A)),"B:"++ (show (rs B)),"C:"++ (show (rs C))]
                  where (SM vf)= eval3 exp
                        (o,rs)=vf state 


-------------------------------------------------------------
-- with exception handling

eval4:: Exp->StateMonad (Maybe Int)
eval4 (V a)=SM (\s -> (return (s a) ,s))
eval4 (Add e1 e2) = do
                    a<-(eval4 e1)
                    b<-(eval4 e2)
                    return ( (+) <$> a <*> b)  
eval4 (Div e1 e2) = do
                    a<- (eval4 e1)
                    b<- (eval4 e2)
                    if b == Just 0 then return Nothing else return (div <$> a <*> b)                  

solve4 exp state= ["output:"++(show o), "A:"++ (show (rs A)),"B:"++ (show (rs B)),"C:"++ (show (rs C))]
                  where (SM vf)= eval4 exp
                        (o,rs)=vf state 
