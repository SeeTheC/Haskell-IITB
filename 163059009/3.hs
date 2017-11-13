import Control.Applicative 
import Control.Monad
import System.Random

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

type State = StdGen
generate::Int->[Int]->StateMonad State [Int] 
generate n r = do
               v1<- pure r
               v2<- (monadicGenerator v1)
               if n /= 0 
               then generate (n-1) v2             
               else return v2

monadicGenerator:: [Int]->StateMonad State [Int]
monadicGenerator v=SM (\sg-> let (rn , newSg)= next sg
                             in  (v++[rn],newSg) )

state:: Int->State
state seed = (mkStdGen seed)
generateRandoms:: Int->Int->[Int]
generateRandoms n seed= fst(g (state seed))
                        where (SM g) =generate n [] 


--fiverandoms = generateRandoms 5 10192

