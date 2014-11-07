# [Writer Monad](https://www.haskell.org/haskellwiki/All_About_Monads#The_Reader_monad)

### Computation type:
produce a stream of data in addition to the computed value.

### Binding strategy:
* A writer monad value: (computation value, log value).
* Binding replaces the computation value with the result of applying the bound function to the previous value and appends any log data from the computation to the existing log data.

### Used for:
* Logging
* other computation that produce output "on the side"

### Defintion:
* multi-parameter type classes and funDeps

      newtype Writer w a = Writer { runWriter :: (a, w)}
      instance (Monoid w) => Monad (Writer w) where
        return a = Writer (a, mempty)
        (Writer (a, w)) >>= f = let (a', w') = runWriter $ f a in Writer (a', w `mappend` w')

* The Writer monad maintains a (value, log) pair, where
the log type must be a **monoid**.
* The return function simply returns the value along
with an empty log.
* Binding executes the bound function using the current
value as input, and appends any log output to the
existing log.


### MonadWriter

     class (Monoid w, Monad m)=> MonadWriter w m | m -> w where
        pass :: m (a, w -> w) -> m a
        listen :: m a -> m (a, w)
        tell :: w -> m ()
