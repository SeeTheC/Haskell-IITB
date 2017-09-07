data Htree a = Null | Fork a (Htree a) (Htree a) deriving (Show,Eq)
-- For converting floating number to Int use Floor or Ceil
l1 = [1,2,3,4,5,6,7]
t1= Fork 11 Null Null

levels l = [  getl i  | i<-[0..(n-1)]]
           where n = floor (log len/ log 2)
                 len= fromInteger$toInteger (length l) + 1 
                 getl i= take ((2^i)) (drop (2^(i)-1) l) 




-- Using Foldl : Top-down
--mktree :: [[a]] -> Htree a
--mktree xss = foldl addLayer Null xss

--addLayer  Null (a:[])  = Fork a Null Null
--addLayer (Fork a lt rt) l= Fork a (addLayer lt (take half l)) (addLayer rt (take half $ drop half l) )
--                           where half= div (length l) 2

-- Using Foldr : Bottom Up


mktree :: (Eq a) =>[[a]] -> [Htree a]
mktree xss = foldr addLayer [Null] xss

--  assuming number of elementsis 2n − 1
addLayer xs t | head t == Null = [ Fork e Null Null | e<-xs]
              | otherwise = [ createNode e i | (e,i)<- zip xs [0..]]  
                where createNode e i = Fork e ((child i)!!0) ((child i)!!1)
                      child i= take 2 $ snd $ splitAt (2*i) t 

