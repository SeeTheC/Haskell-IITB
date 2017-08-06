mydiv x y = mydiv_aux x y (0, x)

mydiv_aux x y (q,r) | r < y = (q,r)
                    | otherwise = mydiv_aux x y (q+1, x - ( (q+1) * y ) )
