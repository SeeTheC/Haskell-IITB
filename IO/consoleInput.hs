main = do
       putStrLn "What is your name?"
       name<-getContents
       putStrLn ("Hey " ++ name ++ "! Welcome to haskell world") 
