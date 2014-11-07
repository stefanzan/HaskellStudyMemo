## Monad Transformers

* The normal Writer monad has two type parameters, so it's more properly written Writer w a. The first parameter w is the type of the values to be recorded, and a is the usual type that the Monad typeclass requires.

* The WriterT transformer has a similar structure, but it adds another type parameter m: this is the underlying monad. The full signature of WriterT is WriterT w m a.

  e.g. WriterT [(FilePath, Int)] IO a

### Functions

#### Control.Monad

    forM_ :: Monad m => [a] -> (a -> m b) -> m ()

    forM :: Monad m => [a] -> (a -> m b) -> m [b]

    when :: Monad m => Bool -> m () -> m ()

    liftM :: Monad m => (a -> r) -> m a -> m r

#### Control.Monad.Trans

    liftIO :: MonadIO m => IO a -> m a

what is MonadIO ?

    class Monad m => MonadIO m where
      liftIO :: IO a -> m a

#### Control.Monad.Writer

    type Writer w = WriterT w Data.Functor.Identity.Identity
    WriterT :: m (a, w) -> WriterT w m a
    tell :: MonadWriter w m => w -> m ()

The difference between Writer and WriterT is that: WriterT is a transformer, thus it has one more type parameter m.

what is WriterT ?

    newtype WriterT w m a = WriterT { runWriterT :: m (a, w)}

what is MonadWriter ?

    class (Monoid w, Monad m) => MonadWriter w m | m -> w where
      writer :: (a, w) -> m a
      tell :: w -> m ()
      listen :: m a -> m (a, w)
      pass :: m (a , w -> w) -> m a

**TODO**: too hard !

What is the difference between Writer, MonadWriter ?

    type Writer w = WriterT w Identity

    class MonadWriter w m ....
      tell :: w -> m ()

Because

    Instance MonadWriter w (WriterT w m)

so the type of tell can be:

    tell :: w -> Writer w m ()

#### WriterT

    runWriterT :: WriterT w m a -> m (a, w)
    execWriterT :: (Monad m) => WriterT w m a -> m w

#### Writer

    writer :: Monad m => (a , w) -> WriterT w m a

Why writer is used to construct a WriterT and the definiton of Writer is based on WriterT ?

    runWriter :: Writer w a -> (a, w)
    execWriter :: Writer w a -> w


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

* The type of liftIO here is :

      liftIO :: IO [String] -> WriterT [(FilePath, Int)] IO [String]

* the type of tell is:

      tell :: [(FilePath, Int)] -> WriterT [(FilePath, Int)] IO ()





### Common patterns in monads and monad transformers

* MonadRader defines the API for the reader monad

* MonadWriter defines the API for the writer monad

#### MonadReader

    class (Monad m) => MonadReader r m | m -> r where
      ask :: m r
      local :: (r -> r) -> m a -> m a
      reader :: (r -> a) -> m a

    instance Monad m => MonadReader r (ReaderT r m)

* type variable r represents the immutable state.
* Reader r monad is an instance of the MonadReader class. as is the ReaderT r m monad transformer.

* the *local* function temporarily modifies the current environment using the r -> r function, and executes its action in the modified environment.

* Question: what is m -> r means in MonadReader ?

  referrr: https://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#functional-dependencies

* Read it later: Type Families

  referrer: https://www.haskell.org/ghc/docs/latest/html/users_guide/type-families.html  

### StateT

     newtype StateT s m a = StateT { runStateT :: s -> m (a, s)}
     type State s = StateT s Identity

### ReaderT

     newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

     type Reader r = ReaderT r Data.Functor.Identity.Identity

### WriterT

     newtype WriterT w m a = WriterT { runWriterT :: m (a, w)}
     type Writer w = WriterT w Identity

### Stacking multiple monad transformers

#### Learn from coding (UglyStack.hs)

* Defining a new type with type keyword.

      type App = ReaderT AppConfig (StateT AppState IO)


You'd better not use : type App2 a = ... a; As it will not support construcing another new Monad Transformer. E.g. WriterT [String] App2 a is not ok.

This is because Haskell does not allow partially apply a type synomym.

* defining the runApp function

      runApp :: App a -> Int -> IO (a, AppState)
      runApp k maxDepth =
          let config = ... -- basic config
              state = ...  -- initial state
          in runStateT (runReaderT k config) state

* You can call the function of StateT at any where in this monad.

    Detail: refer to UnglyStack.hs.

### Large Deriving

      newtype MyApp a = MyA {
        runA :: ReaderT AppConfig (StateT AppState IO) a
      } deriving (Monad, MonadIO, MonadReader AppConfig, MonadState AppState)

** The large deriving clause requires the GeneralizedNewtypeDeriving language pragma**


### Control.Monad.Trans

      class MonadTrans t where
        -- | Lift a computation from the argument monad to the constructed monad.
        lift :: (Monad m) => m a -> t m a

* Every monad transformer is an instance of MonadTrans

* A comparasion

      fmap :: (Functor f) => (a -> b) -> f a -> f b
      liftM :: Monad m => (a -> b) -> m a -> m b
      lift :: (Monad m, MonadTrans t)=> m a -> t m a
