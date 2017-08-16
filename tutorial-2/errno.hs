errno p = findErrorBrace p (0,0)
findErrorBrace [] (open,er) = open+er
findErrorBrace (x:xs) (open,er) | x == '(' = findErrorBrace xs (open+1,er)
                              | x == ')' = if open > 0 
                                           then findErrorBrace xs (open-1,er)
                                           else findErrorBrace xs (open,er+1)

