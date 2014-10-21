## Maybe Monad

* head and tail only work with non-empty lists.

## Motivation Example
An unsafe example

    unsafeLogSqrt = log . sqrt

A safe version:

    safeLog :: (Floating a, Ord a) => a -> Maybe a
    safeLog x
      | x < 0     = Just (log x)
      | otherwise = Nothing

    safeLogSqrt = safeLog <=< safeSqrt.
So compared with unsafeLogSqrt, the only difference is monadic and non-monadic.
One is function composition and the other is monadic composition.

## Another lookup example
    lookup :: Eq a => a -> [(a, b)] -> Maybe b

an example:


    getTaxOwed :: String -> Maybe Double
    getTaxOwed name =
        lookup name phonebook >>= \number ->
        lookup number governmentDatabase >>= \registration ->
        lookup registration taxDatabase
Or use do-block style:

    getTaxOwed name = do
       number <- lookup name phonebook
       registration <- lookup number governmentDatabase
       lookup registration taxDatabase

* what I have learnt form this ?

  Answer: use do-block style is essentially the same as >>=

* small knowledge:

      > :m Data.Maybe
      > :t fromMaybe
      > fromMaybe :: a -> Maybe a -> a
      > fromMaybe 0 (Nothing) = 0
      > fromMaybe 2 (Just 5)  = 5
In Prelude, there is another function called maybe.

      maybe :: a -> (a -> b) -> Maybe a -> b
This meaning of maybe function: the first parameter is for Nothing case, the (a -> b) is for the Just case.

* **return and (>>=) do not allow us to extract the underlying value from a monadic computation.**
typically the IO monad.
