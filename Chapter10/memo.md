# Chapter 14. Monad

## Looking for shared patterns

* A type constructor *m*
* A functin of type: m a -> (a -> b) -> m b. for chaining the output of one function into the input of another.
* A function of type: a -> m a. for injecting a normal value into the chain.

For example, the **Maybe** type:

* type constructor: Maybe a
* chaining function: >>? :: (Maybe a) -> (a -> b) -> (Maybe b)
* injector functio: Just.

## Monad typeclass in standard Prelude

		class Monad m where
			(>>=) :: m a -> (a -> b) -> (m b)
			return :: a -> m a

*>>=* is referred to as "bind", as it binds the result of the computation on the left to the parameter of the one on the right.

The two other functions in Monad

		(>>) :: m a -> m b -> m b
		a >> f = a >>= \_ -> f
		

		fail :: String -> m a
		fail = error
	
**Beware of faile**:
	Many Monad instances don't override the default implementation of fail. 

	
	