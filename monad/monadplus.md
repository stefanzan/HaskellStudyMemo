# MonadPlus
Amagamate all valid solutions into a single result.

My observation from the follow subsections are :
* List MonadPlus return all successful results.
* Maybe MonadPlus return only one, even if both are success.
* Either e is the same as Maybe.

## Definition

    class Monad m => MonadPlus m where
      mzero :: m z
      mplus :: m a -> m a -> m a
* mzero is the monadic value standing for zero results.
* mplus is a binary function which combines two computations.

List MonadPlus

    instance MonadPlus [] where
      mzero = []
      mplus = (++)

Maybe MondPlus

    instance MonadPlus Maybe where
      mzero = Nothing
      Nothing `mplus` Nothing = Nothing
      Nothing `mplus` Just x  = Just x
      Just x  `mplus` Nothing = Just x
      Just x  `mplus` Just y  = Just x

Control.Monad.Error

    instance (Error e) => MonadPlus (Either e) where
      mzero = Left noMsg
      Left _  `mplus` n = n
      Right x `mplus` _ = Right x

**Q**: where use **(Error e) =>** here ?

* Like Maybe, (Either e) represents computations that can fail.
* Unlike Maybe, (Either e) allow the failing computations to include an error "message" (which is usually a String).

## The advantage of monadplus
* e.g. we can use mplus to run two parsers in parallel. That is, we use the result of the first one if it succeeds, and otherwise, we use the result of the second. If both fail, then our whole parser returns Nothing.

## What I learnt from Coding

* Using mplus to combine choices.

      digit :: Int -> String -> Maybe Int
      digit i s
         | if i > 9 || i < 0    = Nothing
         | otherwise            = do
            let (c: _) = s
            if [c] == show i then Just i else Nothing

  this digit function is used to parse a digit i from string s.
  we can use it together with mplus to parse Strings of binary digits.

      binChar :: String -> Maybe Int
      binChar s = digit 0 s `mplus` digit 1 s

## The MonadPlus Laws
**mzero and mplus form a monoid**

* mzero is a neutral element.
      mzero `mplus` m = m
      m `mplus` mzero = m

* mplus is associative.
    (but not all instances obey this law because it makes some infinite structures impossible.)
      m `mplus` (n `mplus` o)  = (m `mplus` n) `mplus` o
**Q**: I did't get the point of but part.

### Additional Laws

    mzero >>= f = mzero
    f >>= mzero = mzero

    (m `mplus` k) >>= k    = (m >>= k) `mplus` (m >>= k)

## Useful functions

### function: msum

    msum :: MonadPlus m => [m a] -> m a
    msum = foldr mplus mzero

    foldr :: (a -> b -> b) -> [a] -> b

**Comment**: nice implementation.

    > msum [Just 1, Just 2, Nothing]
    > Just 1

    > msum [[1],[2]]
    > [1,2]
So, the different behavior is because of the definition of mplus.
Maybe mplus chooses the first one; [] mplus concats all of them.

### function: guard
#### An example of using guard
    pythags = do
      x <- [1..]
      y <- [1..]
      z <- [1..]
      guard (x^2 + y^2 == z^2)
      return (x, y, z)
How it is behaved ?
the type of guard like:

      guard :: MonadPlus m => Bool -> m ()
      guard True = return ()
      guard _    = mzero
If failed, it wil not execute the next return.

Since according to the laws of >>= operation

      mzero >>= m = mzero

an mzero at any point will cause the entire do-block to become mzero.

**Q**: which mzero it is here ?

Is it list monadPlus here ?

## Monoid in Data.Monoid

    class Monoid m where
      mempty :: m
      mappend :: m -> m -> m

The list monoid

    instance Monoid [a] where
      mempty = []
      mappend = (++)

The difference with MonadPlus and Monoid is the usage of [a] and [] in the instance declaration.

** Monoid and MonadPlus work at different levels.**

**Q**: I don't understand the difference between them...
