# Understanding Monads

* *do* is actually just syntatic sugar over monads.

## Definition

three things:

  * a type constructor M;
  * a function return;
  * an operator (>>=) which is pronounced "bind"


    return :: a -> M a
    (>>=) ::  M a -> (a -> M b) -> M b


* (>>) operator is called 'then'.

## Monad laws and one good point

    m >>= return       = m   -- right unit.
    return x >>= f     = f x


  * a good point

        maternalGrandfather p = do
          mom <- mother p
          gf  <- father mom
          return gf

has an simpler version:


        maternalGrandfather p = do
          mom <- mother p
          father mom

This is because of the first law.

## Something about *do* notation.

      bothGrandfathers p = do {
        dad <- father p;
        fg1 <- father dad;
        mom <- mother p;
        gf2 <- father mom;
        return (fg1, gf2);
      }

*father* and *mother* are functions that might fail to produce results.
When that happens, the whole do block will fail. i.e. terminate with an exception.

       let x = foo in x + 3  <====> (\x -> x + 3) foo

       x <- foo; return (x + 3) <=====> foo >>= (\x -> return (x + 3))

## Associativity

    (m  >>= f ) >>= g   =    m >>= (\x -> f x >>= g)

  **Q**:
    why use \x -> fx ?

This is because (you can see from the type definition of >>=)

     (>>=) :: m a -> (a -> m b) -> m b
So, the right hand side of >>= need a function with type : a -> m b.
that is why use \x -> ... to specify it.

##  Monadic composition

    (f >=> g) >=> h =  f >=> (g >=> h)

where (>=>) is analogue of the function composition operator (.).

The type of (>=>) is:

     (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> m c
     f >=> g = \x -> f x >>= g

## Monads and Category Theory

* Monads originally come from a branch of mathematics called Category Theory.

## Start with monads as functors

     fmap   :: (a -> b) -> M a -> M b

     return :: a -> M a
     join   :: M (M a) -> M a

**Q**: frankly speaking, I don't understand *join*.

A functor M can be thought of as container, so that M a contains values of type a.

The bind combinator:

    m >>= g = join (fmap g m)

**Q**: I dont't understand this too.

**A**: I see. I got it. This is because

  1. the type of fmap ::  (a -> b) -> M a-> M b
  2. suppose the type of m is M a.
  3. suppose g :: a -> M b
  4. for fmap g m, substitute b in 1. with M b, the result of fmap is :: (a -> M b) -> M a -> M (M b).
  5. so. fmap g m :: M (M b)
  6. the purpose of join now can be used. join :: M (M a) -> M a
  7. join (fmap g m) :: M b.

Now for m >>= g :: M b. so they are equal.


* Another two:

      fmap f x = x >>= return . f
      join x   = x >>= id

##Deduce: All monads are by definition functors.

Currently, Control.Monad defines liftM, a function with a familiar signature..

     liftM :: (Monad m) -> (a1 -> r) -> m a1 -> m r

liftM is merely fmap implemented with >>= and return.

Memo from: [Haskell wiki book](http://en.wikibooks.org/wiki/Haskell/Understanding_monads)
