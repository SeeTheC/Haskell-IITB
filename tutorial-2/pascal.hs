-- Pascal
pascal = [1]:[  [ a+b | (a,b) <- zip (0:x) (x++[0]) ]  | x<- pascal ]
