# do Notation
* (>>) (*then*) operator works almost identically in do notation and in unsugared code.

**Q**: what is unsugared code ?

**A**: a >> b is unsugared code, using do notation is sugared code.
## then operator

     putStr "hello" >>
     putStr " " >>
     putStr "world!" >>
     putStr "\n"
can be rewritted in do notation:

     do putStr "hello"
        putStr " "
        putStr "world!" >>
        putStr "\n"
## bind operator

    do x1 <- action1
       x2 <- action2
       action3 x1 x2
is equivalent to the following:

    action1 >>= \x1 ->
    action2 >>= \x2 ->
    action3 x1 x2
**chains of lambdas pass the results downstream.**

**Remember that, without extra parenthese, a lambda extends all the way to the end of the expression.**
thus why x1 is still in scope at the point we call action3.

## The fail method
In do block, what happens if action1 returns Nothing ?. Failures are handled with the fail method for the relevant monad.

The do block above translates to:

       action1 >>= f
       where f (Just x1) = do x2 <- action2
                              action x1 x2
             f _         = fail "..."

For instance, Maybe has fail \_ = Nothing; for the list moand, fail \_ = [].

## Just sugar

 snippets like this one are totally redundant.

         fooRedundant = do x <- bar
                           return x
  it equals to : foo = bar

## Learn some good things
### Point 1

       nameReturn :: IO String
       greetAndSeeYou :: IO ()
       greetAndSeeYou = nameReturn >>= \ name -> putStrLN ("See you, " ++ name ++ "!")
so, this code needs a lambda, which is not so good.

suppose we have:

        printSeeYou :: String -> IO ()
        printSeeYou name = putStrLn $ "See you, " ++ name ++ "!"
then, we could write a better code:

        greetAndSeeYou :: IO ()
        greetAndSeeYou = nameReturn >>= printSeeYou

Do yo think it is better ?

**The only thing you need to do is to define a function with type: a -> m b**

### Point 2

If we have a **non-monadic** seeYou fuction:

    seeYou :: String -> String
    seeYou name = "See you, " ++ name ++ "!"
How shall we do ?

We can write:

    -- liftM f m == m >>= return . f == fmap f m
    greetAndSeeYou :: IO ()
    greetAndSeeYou = liftM seeYou nameReturn >>= putStrLn

I will explain the liftM

  According to my understanding, the usefulness of liftM is that:
when you have a pure function, i.e. non-monadic funciton.
You can:
1. lift the value from a monad,
2. compute the result with the pure function.
3. putback to a monad.

That is why we have a pure function *seeYou*, which can be applied to IO monad.
1. First list the String s from IO String;
2. then apply *seeYou* on s.
3. warp it with IO monad.
4. so, the final result is type of : IO String.
5. thus using >>= putStrLn to output the result.


Memo from: [Haskell wiki book](http://en.wikibooks.org/wiki/Haskell/Understanding_monads)
