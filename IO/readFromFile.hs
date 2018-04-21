import System.Environment 

main = do
       args<-getArgs
       content<-readFile $ head args
       -- this will print everything on console but "\n" for enter
       print content
       putStrLn "----------------------"
       -- this will print line by line
       mapM print $ lines content

