newtype MaybeT m a = MaybeT {
  runMaybeT :: m (Maybe a)
}

-- | three standard monad functions
-- maybe :: b -> (a -> b) -> Maybe a -> b
-- the first parameter is the default case.
-- maybe :: Maybe a -> ( a -> Maybe a) -> Maybe a -> Maybe a
-- | maybe is used to get the inner value of a Maybe monad and computed to another monad type.

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
bindMT mTa f = MaybeT $ runMaybeT mTa >>= maybe (return Nothing) (runMaybeT . f)

returnMT :: Monad m => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

failMT :: (Monad m ) => a -> maybeT m a
failMT _ = MaybeT $ return Nothing

instance (Monad m) => Monad (MaybeT m) where
  return = returnMT
  (>>=) = bindMT
  fail = failMT

-- lift :: m a -> m (Maybe a)
instance MonadTrans MaybeT where
  lift m = MaybeT $ liftM Just m

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO :: IO a -> MaybeT m a
  liftIO m = lift (liftIO m)
  -- IO a -> m a ->  MaybeT $ m (Maybe a)

-- | quite hard ....
instance (MonadState s m) => MonadState s (MaybeT m) where
  get = lift . get
  put k = lift (put k)


instance (MoandWriter m) => MoandWriter w (MaybeT m) where
  tell = lift . tell
  listem m = MaybeT $ do
    (result, log) <- listen (runMaybeT m)
    case result of
      Nothing -> return Nothing
      Just value -> return (Just (value, log))
  pass m = MaybeT $ do
    result <- runMaybeT m
    case result of
      Nothing -> return Nothing
      Just (value, log) -> pass (return (Just value, log))     



