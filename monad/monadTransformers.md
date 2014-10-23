# Monad transformers

The motivation is to use several monads at once.

## some library functions

Dat.Maybe

     isJust :: Maybe a -> Bool
     isNothing :: Maybe a -> Bool

## Example: MaybeT

    newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a)}

    instance Monad m => Monad (MaybeT m) where
      return = MaybeT . return . return

### return

  Now let me explain my understanding of the return for MaybeT m. Let me first re-explain the normal Monad instance.

      class CName a where
        ....

      instance Monad m where
        return :: a -> m a
        (>>=) :: m a -> (a -> m b) -> m b
    So, the a is infact the type parameter of the typeclass m. If m in  *Monad m* is CName, then the a in return function is the a in *CName a*.

  Thus for the Monad instance *Monad (MaybeT m)*, the type of return is

      return :: a -> MaybeT m a

  Interesting !

  First, in the implementation of return, the right-most return is the return of Monad Maybe with type:

      return :: a -> Maybe a
  Then, the middle return is the return of Monad m with type:

      return :: (Maybe a) -> m (Maybe a)

  Finally, using the newtype wrapper MaybeT to wrap the result.

      MaybeT :: m (Maybe a)  -> MaybeT m a

### (>>=)

First recall the definition of MaybeT, and we add the implementation of >>=.

    newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a)}

    instance Monad m => Monad (MaybeT m) where
        return = MaybeT . return . return
        x >>= f = MaybeT $ do maybeValue <- runMaybeT x
                              case maybeValue of
                                Nothing  -> return Nothing -- m (Maybe a)
                                Just x   -> runMaybeT $ f x -- f x :: MaybeT m a


* runMaybeT :: MaybeT m a -> m (Maybe a)

  this is used to unwrap the m (Maybe a) from newtype MaybeT

* MaybeT :: m (Maybe a) -> MaybeT m a

  this is used to construct a MaybeT m a from m (Maybe a).

* **Why use the MaybeT constructor before the do bloack while we have accessor runMaybeT within do ?**

  The do block must be in the m monad, not in MaybeT m. Because the MaybeT m lacks a defined bind operator at this point.

 ## One hard point: MonadTrans

      instance MonadTrans MaybeT where
         lift =  MaybeT . liftM  Just  
My explanation is :
* be aware of the line of instance MoandTrans ... where. You will see it is different from previous. Here only use MaybeT instead of MaybeT m.

  This is because Monad and MonadPlus can only accept a type with only one type parameter.

  But MonadTrans has no limitation. (my guess)

  so the value is m (Maybe a). Thus firstly using liftM is for m. Since insdie m is a Maybe monad, so use Just to wrap, I think it is the same as return. (my guess too)

* Let me explain the lift function here.

  lift is used to compute on the wrapped value and wrap it with the type. (my guess)

  -- **wrong**, please see the type definition below.

      lift :: (Monad m, MonadTrans t) => m a -> t m a

  From [hackage Control.Monad.Trans.Class](http://hackage.haskell.org/package/transformers-0.4.1.0/docs/Control-Monad-Trans-Class.html#t:MonadTrans)  

**Q**: what is the difference between lift and liftM ?

    liftM :: Monad m => (a1 -> r) -> m a1 -> m r -- Control.Monad

**Q**: according to this, why not implement lift like this:

    lift = MaybeT **!!!**

I think this is because: instance MonadTrans MaybeT where ...
means you give me a m with inner type a, I try to manage to get a MaybeT m a. So
you need to find a way to use a

--   lift = MaybeT .  liftM Just

**Comment**: I still don't think liftM is correct here. As the input of liftM is *m a1*.


**mark**： got it. [2014-10-22 10:56]

Let me explain to you:

* First, the input of lift is: m a.
* Second, for implementing lift for MaybeT, we shall also suppose the input type is: m a
* liftM Just :: (a -> Maybe a) -> m a -> m (Maybe a)
* Finally, get with type: MaybeT m a.

No.... If the input is m a, the output of lift shall be t m a.
But here the input is m a, while the output is t m (Maybe a)

**Unsolved**

The only possible reason is:

    lift m = MaybeT $ m >>= liftM Just

**TODO**

source: [wiki book](http://en.wikibooks.org/wiki/Haskell/Monad_transformers)


## Jump
**TODO**: read later

## liftM

    liftM :: Monad m => (a -> b) -> m a -> m b

Two equal pieces of code:

    do x <- monadicValue
       return (f x)

And

    liftM f monadicValue

And

    f `liftM` monadicValue

## lift

** use lifting operations to bring functions from the inner monad into the combined monad.**

     class MonadTrans MaybeT where
       lift :: (Monad m) => m a -> t m a


Another special case:

Control.Monad.IO.Class

     class (Monad m) => MonadIO m where
        liftIO :: IO a -> m a

**Q**: not so clearly understood yet.. liftIO


    instance MonadTrans MaybeT where
       lift m = MaybeT (m >>= return . Just)

This is correct if I am thinking in this way:

       lift m = MaybeT $  m >>= \x ->
                          return . Just x

Which means x is the inner value with type a, not in m (Maybe a) type.
Yes, you are right !!

    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b


## summarise of thinking

   two different implementations of lift for MaybeT in MonadTrans

    1. lift = MaybeT . liftM Just

    2. lift m = MaybeT (m >>= return . Just)

As the type of lift:: m a -> t m a, means wrapping m a with another t.

For the second, it equals to:

    lift m = MaybeT (m >>= \x -> return . Just x)

First wrap x with Maybe, then use return to wrap to m (Maybe a).

For the first, it equals to:

    lift m = MaybeT $ (liftM Just m)

m is the type: m (Maybe a),
so the type of liftM is :: (a  -> Maybe a) -> m (Maybe a) -> m (Maybe (Maybe a)) ????

-- 2014.10.23 After discussing with Dr. Josh, I found my mistake.

**<span style="color:red">the type of m is not m (Maybe a)</span>**

**The type of m is m a**
