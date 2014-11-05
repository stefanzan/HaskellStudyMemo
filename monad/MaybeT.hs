newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a)}

instance Monad m => Monad (MaybeT m) where
--  return = MaybeT . return . Just
  return = MaybeT . return . return
--  (>>=) :: MaybeT m a -> ( a -> MaybeT m b) -> MaybeT m b
--  x >>= f = MaybeT $
--    runMaybeT x >>= \maybeValue -> | Nothing = return Nothing
--                                   | Just v  = runMaybeT $ f v
  x >>= f = MaybeT $ do maybeValue <- runMaybeT x
                        case maybeValue of
                            Nothing -> return Nothing -- m (Maybe a)
                            Just x  -> runMaybeT $ f x -- f x :: MaybeT m a

instance Monad m => MonadPlus (MaybeT m) where
  mzero = MaybeT $ return Nothing
  mplus x y = MaybeT $  do maybeValue <- runMaybeT x
                           case maybeValue of
                             Nothing -> runMaybeT y
                             Just _  -> return maybeValue

instance MonadTrans MaybeT where
  lift = MaybeT . (liftM Just)
     
