solution 0 b c 	| (mod c b) == 0 = [0, div c b]
		| otherwise = [-1,-1] -- Error. Solution does not exist

solution a b c  = [x,y]
		 where 	g = solution (mod b a) a c
			x1=g!!0
			y1=g!!1
			x=y1-(div b a)*x1
			y=x1
