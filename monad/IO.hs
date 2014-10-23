module Main where

import Data.Char (toUpper)
import Control.Monad

main = putStrLn "Write you string: " >> liftM shout getLine >>= putStrLn

shout = map toUpper

speakTo :: (String -> String) -> IO String
speakTo f = liftM f getLine

sayHello :: IO String
sayHello :: speakTo (\name -> "Hello, " ++ name ++ "!")
