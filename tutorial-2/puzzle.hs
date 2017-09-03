good_nums = [2..99]::[Int]
good_factors p = [(a,b) | a<-good_nums, b<-good_nums, a*b==p,a<=b]
good_summands s = [(a,b) | a<-good_nums, b<-good_nums, a+b==s,a<=b]

singleton l = s == 1
              where s=foldr f 0 l
                    f x y = if x == False then y else y+1
fact1:: (Int,Int) -> Bool
fact1 (a,b) =   (length.good_factors) (a*b) > 1

fact2:: (Int,Int) -> Bool
fact2 (a,b) =   (length.good_summands) (a+b) > 1

fact3:: (Int,Int) -> Bool
fact3 (a,b) =   (and.map fact1.good_summands) (a+b)

fact4:: (Int,Int) -> Bool
fact4 (a,b) =   (singleton.map fact3.good_factors) (a*b)


fact5:: (Int,Int) -> Bool
fact5 (a,b) =   (singleton.map fact4.good_summands) (a+b)

                 
result = [(a,b) | a<-good_nums, b<-good_nums, fact1 (a,b),fact2 (a,b),fact3 (a,b),fact4 (a,b),fact5 (a,b) ]

