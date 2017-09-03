-- (**) Drop every N'th element from a list.
dropEvery [] _ =[]
dropEvery l n = (take (n-1) l) ++ dropEvery (drop n l) n
