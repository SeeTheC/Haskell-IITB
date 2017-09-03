-- A EMI is the monthly payment such that the balance is 0 after the given time period.


balance pr r y mp | y == 0 = pr
                  | otherwise= balance endBal r (y-1) mp
                    where i=pr*(r/100)
                          nb=pr+i
                          endBal=nb-(mp*12)



emi pr r y = zero (balance pr r y) 

-- it will tell when function will give the o/p as zero. 
-- function is the function of one varibable
-- using gradient descent method 
zero1 f xi   | f xi == 0 = 0
             | otherwise=if smallchange then xinew else zero1 f xinew
               where xinew = xi - f xi/diff f xi 
                     smallchange = abs(f xinew)<0.0001


-- derivative
diff f x =  ( f (x+delta) - f x ) / delta
             where delta = 0.00001 

-- using until
zero f = until pred improve initial
          where improve xi = xi - f xi/diff f xi
                pred xi= abs (f xi) < 0.0001
                initial = 1

