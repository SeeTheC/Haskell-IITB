--(**) Eliminate consecutive duplicates of list elements.
compress l = foldr (\x o -> if (not.null) o && x == (head o) then o else (x:o) ) [] l
