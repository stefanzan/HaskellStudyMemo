# List

**As monads, lists are used to model nondeterministic computations which may return an arbitrary number of results.**

## List instantiated as monad

* return

      return :: a -> [a] / a-> [] a
      return x = [x]
* bind
      bind :: [a] -> (a -> [b]) -> [b]
  so we need a function f :: a -> [b]. get one element from a list, result in a list [b].
      xs >>= f =  concat (map f xs)
## An example
      replicate :: Int -> a -> [a]
      let generation = replicate 3
      generation :: a -> [a]
since generation is the type from a -> [a] which matches the function in the bind.
so we can do the following:
      > ["bunny"] >>= generation
      > ["bunny", "bunny", "bunny"]
      > ["bunny"] >>= generation >>= generation
      > ["bunny", "bunny", "bunny", "bunny", "bunny", "bunny", "bunny", "bunny", "bunny"]
**This is a new. previously we only consider one thing insider the monad container, now in side have a list of elements.
it will apply the function a->[b] for each element inside.
**
