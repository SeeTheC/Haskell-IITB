import System.Environment

main = do
       args<-getArgs
       print args
       content<-readFile (head args)
       print $ addBlackLine content
       writeFile "temp.txt" (addBlackLine content)


addBlackLine content = unlines [ l++"\n" |  l <-lines content]
