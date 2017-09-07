-- Generalized Horner’s rule:
-- 1 + x0 + x0 ∗ x1 + x0 ∗ x1 ∗ x2 + x0 ∗ x1 ∗ x2 ∗ x3
h1 l = foldl (+) 0 (scanl (*) 1 l )

-- = 1 + x0 ∗ (1 + x1 ∗ (1 + x2 ∗ (1 + x3 )))

h2 l = foldr (\x o-> 1+x*o) 1 l
