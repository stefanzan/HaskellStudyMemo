data MMaybe a = MNothing
             | MJust a

return :: a -> MMaybe a
return x = MJust x

(>>=) :: MMaybe a -> (a -> MMaybe b) -> MMaybe b
m >>= g = case m of
            MNothing -> MNothing
            MJust x -> g x

class MMonad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  fail :: String -> m a
  