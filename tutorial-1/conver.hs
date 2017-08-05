convert  n  = conv l
              where l = toSixDigitString (show n )

toSixDigitString s = if (length s) < 6
                     then toSixDigitString ("0" ++ s)
                     else s
-- xxx or xxxY
conv (x:y:z:ls) | ls == [] = f
                | ls /= [] =  if ( (length k) /= 0  && (length g) /= 0)
                              then k ++ " and " ++ g
                              else if ( (length g) == 0 && (length k) /= 0)
                              then k
                              else if ((length k) == 0 && (length g) /= 0)
                              then g
                              else "zero"

                where twoDigit = (read [y] :: Int ) * 10 + (read [z] :: Int )  
                      remaining  = (conv ls)                      
                      f1 = (str (read [x] :: Int)) ++ " " ++ (str 100) 
                      f  = if ( ( x==y && y==z && (read [y] :: Int ) == 0 ) )  -- All are zero
                           then ""
                           else if ( (read [x] :: Int ) == 0  )
                           then conv (y:[z])
                           else if ( y==z && (read [y] :: Int ) == 0 )  -- Three digit
                           then f1
                           else if ( (read [z] :: Int ) == 0 ) 
                           then f1 ++ " and " ++ ( str ((read [y] :: Int) * 10) )
                           else if twoDigit < 20
                           then f1 ++ " and " ++ (str twoDigit )
                           else f1 ++ " and " ++ (str ((read [y] :: Int) * 10) ) ++ " " ++ (str (read [z] :: Int))
                      g  = if( (length remaining) /= 0 )
                           then remaining
                           else  ""
                      k  = if( (length f) /= 0 )
                           then f ++ " " ++ (str 1000)
                           else  ""

-- xx or xxXXX
conv (x:y:ls)   | ls == [] = f
                | ls /= [] = f ++ " " ++ (str 1000) ++ " and " ++ conv ls
                where twoDigit = (read [x] :: Int ) * 10 + (read [y] :: Int )  
                      f  = if ( x==y && (read [y] :: Int ) == 0 )  -- All are zero
                           then ""
                           else if ( (read [x] :: Int ) == 0 )
                           then conv [y]
                           else if ((read [y] :: Int ) == 0 ) 
                           then ( str ((read [x] :: Int) * 10) )
                           else if twoDigit < 20
                           then (str twoDigit )
                           else (str ((read [x] :: Int) * 10) ) ++ " " ++ (str (read [y] :: Int))
-- x or xXXX
conv (x:ls)     | ls == [] = f
                | ls /= [] = f ++ " " ++ (str 1000) ++ " and " ++ conv ls
                where f  = str (read [x] :: Int)
                      num = (read [x] :: Int)

-- Empty		   
conv (ls) =  "" 

str n  =  case n of     0 -> "ZZZero"
                        1 -> "one"
                        2 -> "two"
                        3 -> "three"
                        4 -> "four"
                        5 -> "five"
                        6 -> "six"
                        7 -> "seven"
                        8 -> "eight"
                        9 -> "nine"
                        10 -> "ten"
                        11 -> "eleven"
                        12 -> "twelve"
                        13 -> "thirtheen"
                        14 -> "fourtheen"
                        15 -> "fifteen"
                        16 -> "sixteen"
                        17 -> "seventeen"
                        18 -> "eighteen"
                        19 -> "nighteen"
                        20 -> "twenty"
                        30 -> "thirty"
                        40 -> "fourty"
                        50 -> "fifty"
                        60 -> "sixty"
                        70 -> "seventy"
                        80 -> "eighty"
                        90 -> "ninty"
                        100 -> "hundred"
                        1000 -> "thousand"
