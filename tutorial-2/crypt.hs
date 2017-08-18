
-- Designed to solve any crypt problem
crypt = rmDuplicate ( concat (reverse (concat ans)) )
        where l=map (\(x,y,z)->[x,y,z]) (zip3 "#send" "#more" "money")
              ans=(take 1 (crypt_aux l) )
              rmDuplicate l=foldl (\y x -> if not (x `elem` y) && (fst x) /= '#' then y++[x] else y) [] l
       

-- i/p: [ ['#','#','m'],['s','m','o'] ,['e','o','n'],['n','r','e'],['d','e','y'] ]
-- o/p: [ [(Char,Int)]]-- all correct ans to it. For this question one unique solution exits
crypt_aux [] = [[]]
crypt_aux (x:ls)  = [   ans ++ [(zip x d)]   |  ans<- (crypt_aux ls)  ,  d<-[ [a,b,c]  | a<-[0..9],b<-[0..9],c<-[0..9]], 
                                               -- Guard condition
                                               checkPairCorrect x d, -- checks char and number pair correct 
                                               uniqueness (zip x d) (concat ans),-- checks same 'char' should have same value 
                                               uniqueNum (zip x d) (concat ans), -- checks number alloted should not be used
                                               validate (ans ++ [(zip x d)] ) 0]



--------------------------------------------------- [Guard condition]--------------------------------------------------------
checkPairCorrect x y = and [(c 0 1),(c 1 2),(c 2 0)] 
                       where c i j= if x!!i == x!!j then y!!i == y!!j else y!!i /= y!!j


-- i/p  [ (a,t1),(b,t2),(c,t3)] [ ('d',1),('e',2),('y',3) ]
uniqueness [] _ = True
uniqueness _ [] = True
uniqueness ((a,t):as) ls = if (null el) then (uniqueness as ls) 
                           else if (snd (el!!0)) == t then (uniqueness as ls) 
                           else False
                           where el=filter (\(x,y)-> if a == x then True else False) ls

-- i/p  [ (a,t1),(b,t2),(c,t3)] [ ('d',1),('e',2),('y',3) ]
uniqueNum [] _ = True
uniqueNum _ [] = True
uniqueNum ((a,t):as) ls =  if (null el) then (uniqueNum as ls) 
                           else if (fst (el!!0)) == a then (uniqueNum as ls) 
                           else False
                           where el=filter (\(x,y)-> if t == y then True else False) ls

-- Checkes addition is correct
validate [] _ = True
validate ([(a,p),(b,q),(c,r)]:sol) carry =  if a=='#' 
                                            then r == carry && carry==1
                                            else ( if (p + q + carry) `mod` 10 == r then validate (sol) ((p + q) `div` 10) else False )
