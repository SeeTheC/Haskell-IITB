data Exp a = Con a | Add (Exp a) (Exp a) | Div (Exp a) (Exp a) deriving (Show,Eq)

exp1 = Add (Con 2) (Con 3)
exp2 = Div  (Con 2)  (Con 0)
exp3 = Div  (Con 12)  (Con 5)

eval1 (Con a)= a
eval1 (Add e1 e2)= (eval1 e1) + (eval1 e2)
eval1 (Div e1 e2)= (eval1 e1) `div` (eval1 e2)


eval2 (Con a)= Just a
eval2 (Add e1 e2)= case (eval2 e1) of
                        Nothing ->  Nothing
                        Just a -> case (eval2 e2) of
                                     Nothing -> Nothing
                                     Just b -> Just (a + b)

eval2 (Div e1 e2)= case (eval2 e1) of
                        Nothing ->  Nothing
                        Just a -> case (eval2 e2) of
                                     Nothing -> Nothing
                                     Just b -> if b==0 then Nothing else Just (a `div` b)



-- instance Functor Exp where
--     fmap f (Con a)= Con (f a)
--     fmap f (Add e1 e2) = Add (f <$> e1) (f <$> e2)
--     fmap f (Div e1 e2) = Div (f <$> e1) (f <$> e2)
 
-- instance Applicative Exp where 
--      pure a = Con a
--      (<*>) (Con f) av= f <$> av
--      (<*>) (Add ef1 ef2) av= (Add (ef1 <*> av) (ef2 <*> av))
--      (<*>) (Div ef1 ef2) av= (Div (ef1 <*> av) (ef2 <*> av)) 

--instance Monda Exp where 
--      return = pure
--      (>>=) mv fm = 


eval3 (Con a)= Just a
eval3 (Add e1 e2)= (eval3 e1) >>= (\a-> (eval3 e2) >>= (\b-> return (a + b) ) )
eval3 (Div e1 e2)= (eval3 e1) >>= (\a-> (eval3 e2) >>= (\b-> if b==0 then Nothing else return (div a b) ) )


eval4 (Con a)= Just a
eval4 (Add e1 e2)= do
                   a<- (eval4 e1)
                   b<- (eval4 e2)
                   return (a + b)

eval4 (Div e1 e2)= do
                   a<- (eval4 e1)
                   b<- (eval4 e2)
                   if b==0 then Nothing else return (div a b) 

