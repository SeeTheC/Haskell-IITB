main = do 
       input<-getContents
       putStr $ addLineNumber input

-- giving line number to each line
addLineNumber input = fst $ foldl f ("",0) allLines
                      where 
                      allLines=lines input
                      f (pv,ln) cv= (pv ++ (show $ ln + 1) ++ " | " ++ cv ++ "\n", ln+1)
                      
                             
