class (Functor n) => NewApplicative n where
    (<**>) :: (Functor m) => m (a -> b) -> n a -> n a

instance NewApplicative Maybe where
      f <**> box =  box

