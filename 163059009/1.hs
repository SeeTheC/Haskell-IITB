data LogSqrt = Val Float | Log LogSqrt | Sqrt LogSqrt

e0 = Val 0
e1 = Val 1.0
e2 = Val 2.0
exp1= Sqrt (Log e0)

eval:: LogSqrt-> Maybe Float
eval (Val a)=return a
eval (Log exp)= do
              x<-eval exp
              if (x<0) then Nothing else return (log x)

eval (Sqrt exp)= do
                 x<- eval exp
                 if(x<0) then Nothing else return (sqrt x)
