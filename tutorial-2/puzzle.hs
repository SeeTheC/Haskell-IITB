good_nums = [2..99]::[Int]
good_factors p = [(a,b) | a<-good_nums, b<-good_nums, a*b==p,a<=b]
good_summands s = [(a,b) | a<-good_nums, b<-good_nums, a+b==s,a<=b]
--singleton l | l == n
fact1 (a,b) =   (length.good_factors) (a*b) > 1
fact2 (a,b) =   (length.good_summands) (a+b) > 1
fact3 (a,b) =   (and.map fact1.good_summands) (a+b)
fact4 (a,b) =   (and.map fact3.good_factors) (a*b)
                 
result = [(a,b) | a<-good_nums, b<-good_nums ]
