ak_mult x y = solve x y 0

solve x y n 	| x==1  = y+n
		| odd x = n + (solve x1 y1 y)
		| otherwise = n + ( solve x1 y1 0)
		where	x1= div x 2
			y1=y*2 
