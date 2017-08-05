modexp x y n = solve x y 1 n

solve x y t n	| y == 0 = 1
		| y == 1 = (x * t) `mod` n
		| even y = solve x1 ye t n
		| odd y  = solve x1 yo t1 n
		where 	x1=(x*x) `mod` n
			ye= div y 2
			yo= div (y-1) 2
			t1= mod (t*x) n
