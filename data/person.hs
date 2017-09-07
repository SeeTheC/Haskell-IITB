data Person = Person {age::Int,name::String} deriving (Show)

data StrPerson = StrPerson {sage::Int,sname::String}	

instance Show StrPerson where
           show StrPerson {sage=a,sname=n} = n ++ " has age " ++ show a ++ "." 
