coeff a b  | a == 0 = [0,1]          
	   | otherwise = [x,y]
			 where  g=coeff (mod b a) a
			        x1=g!!0
				y1=g!!1
				x=y1 - (div b a) * x1
				y=x1
