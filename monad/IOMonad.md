# I/O
## subtle point
In other programming languages, which do not have I/O actions,

        speakTo :: (String -> String) -> String

**With such type, however speakTo would not be a function at all !    **
* Functions produce the same result when given the same arguments.

I think it is not clear ! The in fact reason is for an I/O, the function itself is
not changed, while the state of the universe world changed.

## IO primitives

* putStrLn
* getLine

## Monadic control structures
* monadic values: just like any other values in Haskell.

      sequence :: (Monad m ) => [m a] -> m [a] -- Prelude

      Prelude Data.Maybe> sequence [Just 1, Nothing, Just 2]
      Nothing
      Prelude Data.Maybe> sequence [Just 1,  Just 2]
      Just [1,2]

      >:t replicate
      > replicate :: Int -> a -> [a]
      > let fiveGetLines = replicate 5 getLine
      >:t
      > fiveGetLines :: [IO String]

  **suquence and replicate form an appealing combination**
* Control.Monad offers replicateM.

  And monadic zips, folds, ...
      replicateM:: Int -> m a -> m [a]


**TODO:** to be learnt

## Comparison normal and monadic value functions

      map   :: (a -> b) -> [a] -> [b]
      mapM  :: (a -> m b) -> [a] -> m [b]
which means if using map with a f :: a -> m b, would result in [m b].
so providing mapM to get the list of m [b].

## There are some functions with _ mark.
* sequenceM_
* mapM_ :: (Monad m) => (a -> m b) -> [a] -> m ()
* replicateM_


Memo From: [Haskell Wiki Book](http://en.wikibooks.org/wiki/Haskell/Understanding_monads/IO)
