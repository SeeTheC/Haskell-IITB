--The Josephus problem: n people numbered 1 to n are made to stand in a
--circle. Starting from the person numbered 1, every third live person is
--killed.

--Write a function (canSurvive pos n) that takes as its arguments a
--position pos and the number of people n, and returns True if the person
--at position pos is one of the last two survivors otherwise it returns False.

canSurvive pos n  |  n <=2  = True
                  |  pos == 3 = False
                  |  otherwise = canSurvive ((pos-3) `mod` n) (n-1)
