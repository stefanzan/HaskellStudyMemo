* Control.Monad.Trans.Reader

      newtype ReaderT r m a = { runReaderT :: r -> m a}

      ask :: (Monad m) => ReaderT r m r
      ask = ReaderT return

    The type of return is ?

      return :: r -> m r

 So the really meaning of ask is : you give me the environment.

      type Reader r = ReaderT r Identity
      reader :: (Monad m) => (r -> a) -> ReaderT r m a
      reader f = ReaderT (return . f)

  The meaning of reader is : using  f :: r -> a. Then using return to get m a. Finally using ReaderT to construct ReaderT r m a.

  Or thinking in another way: return . f :: r -> m a.

      local :: (Monad m) => (r -> r) -> ReaderT r m a -> ReaderT r m a
      local = withReaderT

      withReaderT :: (r -> r') -> ReaderT r m a -> Reader r' m a
      withReaderT f m =  ReaderT $ runReaderT m . f


* Control.Monad.Reader:

      newtype Reader r a = Reader { runReader :: r -> a}

Suppose we have a reader readerM, if you give me the environment r. I will give you the a you want by using

      runReader readerM r

ReaderT is the Monad Transformer for Reader. Suppose we have a reader transformer readerTM, if you give me an environment r, I will give you the value wrapped by the monad m.

      newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a}

      runReaderT readerTM r :: m aM

* Control.Monad.Reader.Class

      class Monad m => MonadReader r m | r -> m where
        ask :: m r
        ask = reader id
        local :: (r -> r) -> m a -> m a
        reader :: (r -> a) -> m a
        reader f = do
          r <- ask
          return $ f r

We define a Monad for Reader, we call it MonadReader.


* ReaderT r m is an instance of MonadReader r m, so as Reader r

      instance Monad m => MonadReader r (ReaderT r m)

      instance MonadReader r (Reader r) where
         ask = Reader id
         local f m = Reader $ runReader m . f
