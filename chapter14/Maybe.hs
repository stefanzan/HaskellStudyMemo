data Maybe a = Nothing
             | Just a

chain :: m a -> (a -> b) -> m b
inject :: a -> m a
