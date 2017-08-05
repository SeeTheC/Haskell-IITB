factorial 0 = 1
factorial n = n*factorial (n-1)

factorial_1 n 	| n==0 = 1
		| otherwise = n * factorial_1 (n-1)	
