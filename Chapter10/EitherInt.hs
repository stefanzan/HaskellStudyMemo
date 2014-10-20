{-# LANGUAGE FlexibleInstances #-}
instance Functor (Either Int) where
	fmap _ (Left l) = Left l
	fmap f (Right r) = Right (f r)
