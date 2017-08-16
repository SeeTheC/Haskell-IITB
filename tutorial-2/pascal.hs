pascal = [1]:[ pNextLevel x | x<- pascal]
pNextLevel clevel = [x+y| (x,y) <- zip (0:clevel) (clevel++[0]) ]
