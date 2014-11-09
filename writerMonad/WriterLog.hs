import Data.Monoid

--applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
--applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

isBigBang :: Int -> (Bool, String)
isBigBang x = (x > 9, "Compared with size to 9")

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)



type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _       = ("beer", Sum 30)

-- | example 2

newtype Writer w a = Writer { runWriter :: (a, w)}

instance (Monoid w) => Monad (Writer w) where
  return a = Writer (a, mempty)
  (Writer (x, v)) >>= f = let Writer (y, v') = f x in Writer (y, v `mappend` v')

logNumber :: Int -> Writer [String] Int
logNumber x = Writer (x, ["Got Number: " ++ show x])

tell :: w -> Writer w ()
tell w = Writer ((), w)

mulWithLog :: Writer [String] Int
mulWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["goona multiply"]
  return (a*b)


gcd' :: Int -> Int -> Writer [String] Int
gcd' a b 
  | b == 0 = do
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      gcd' b (a `mod` b)
