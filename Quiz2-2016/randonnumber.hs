import Control.Monad.State
import System.Random
--fiverandoms = generateRandoms 5 10192


generateRandoms n is=  evalState (generateRandoms' n) (mkStdGen is)

generateRandoms':: Int -> State StdGen [Int]
generateRandoms' n = do
                     s<-get
                     if n==0 then return []
                     else do
                          rn<- return (fst (next s) )
                          put (snd (next s))
                          xs<-generateRandoms' (n-1)
                          return (rn:xs)
                      
