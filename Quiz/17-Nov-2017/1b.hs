-- Find Type

-- (Num a)=> t1 -> t2 -> a
-- t1 :: f 'a' :: char
-- f 'a' :: a -> a :: char->char
-- t2 :: f "a" :: [char]
-- f "a" :: a -> a :: [char]->[char]

-- ^ above thing is wrong

-- let f=\x -> x
--    g a b = 3
-- in  g (f 'a') (f "a")

-- this will give syntax error
