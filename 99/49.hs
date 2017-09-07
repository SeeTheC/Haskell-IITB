-- https://wiki.haskell.org/99_questions/46_to_50
-- (**) Gray codes.
--An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,

--n = 1: C(1) = ['0','1'].
--n = 2: C(2) = ['00','01','11','10'].
--n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].

-- > gray 3
-- >["000","001","011","010","110","111","101","100"]

gray 0 = [""]
gray n = map ('0':) l ++ map ('1':) (reverse l)
        where l=gray (n-1) 
