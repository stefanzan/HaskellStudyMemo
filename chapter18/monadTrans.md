## Monad Transformers

* The normal Writer monad has two type parameters, so it's more properly written Writer w a. The first parameter w is the type of the values to be recorded, and a is the usual type that the Monad typeclass requires.

* The WriterT transformer has a similar structure, but it adds another type parameter m: this is the underlying monad. The full signature of WriterT is WriterT w m a.

  e.g. WriterT [(FilePath, Int)] IO a

### Functions

#### Control.Monad

    forM_ :: Monad m => [a] -> (a -> m b) -> m ()

    when :: Monad m => Bool -> m () -> m ()

    liftM :: Monad m => (a -> r) -> m a -> m r

#### Control.Monad.Trans

    liftIO :: MonadIO m => IO a -> m a

what is MonadIO ?

    class Monad m => MonadIO m where
      liftIO :: IO a -> m a

#### Control.Monad.Writer

    WriterT :: m (a, w) -> WriterT w m a
    tell :: MonadWriter w m => w -> m ()

what is WriterT ?

    newtype WriterT w m a = WriterT { runWriterT :: m (a, w)}

what is MonadWriter ?

    class (Monoid w, Monad m) => MonadWriter w m | m -> w where
      writer :: (a, w) -> m a
      tell :: w -> m ()
      listen :: m a -> m (a, w)
      pass :: m (a , w -> w) -> m a

**TODO**: too hard !



#### WriterT

    runWriterT :: WriterT w m a -> m (a, w)
    execWriterT :: (Monad m) => WriterT w m a -> m w


### Learning from Code

    countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
    countEntries path = do
      contents <-  liftIO . listDirectory $ path
      tell [(path, length contents)]
      forM_ contents $ \name -> do
        let newName = path </> name
        isDir <- liftIO . doesDirectoryExist $ newName
        when isDir $ countEntries newName

* The composition operator (.) and ($) operator in line 3.
You could not remove the ($). If removing, then it is incorrect. Since the composition needs the two arguments to be a function.

* ($)的优先级低于(.)

### Common patterns in monads and monad transformers

#### MonadReader

    class (Monad m) => MonadReader r m | m -> r where
      ask :: m r
      local :: (r -> r) -> m a -> m a

* type variable r represents the immutable state.
* Reader r monad is an instance of the MonadReader class. as is the ReaderT r m monad transformer.

* the *local* function temporarily modifies the current environment using the r -> r function, and executes its action in the modified environment.


### StateT

     newtype StateT s m a = StateT { runStateT :: s -> m (a, s)}

### ReaderT

     newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

### WriterT

     newtype WriterT w m a = WriterT { runWriterT :: m (a, w)}


### Stacking multiple monad transformers
