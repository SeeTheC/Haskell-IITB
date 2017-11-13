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

type State = [String]

state1 = []

-- finds the identifier
matches:: String -> (String,String)
-- writing raw code. NOT Absolutly correct
matches "" =  ("","")
matches str = ([head str], tail str) -- work if all

--type State= String -> (String,String)

---state1:: State
--state1 str = ("",str)

--identifiers:: String->StateMonad [String] String
--identifiers str = pure str >>= monadicMatch                
monadicMatch:: String->StateMonad [String] String
monadicMatch v = SM(\s-> let (id,substr)=matches v
                         in (substr,s++[id]))                 

identifiers:: String->StateMonad [String] String
identifiers str = do 
                  v1<-pure str 
                  v2<-monadicMatch v1
                  if (length $ fst $ matches v2) /= 0 
                  then identifiers v2
                  else return v2

solve state str =  (substr,ids)
                   where (SM f)=identifiers str 
                         (substr,ids)=f state
