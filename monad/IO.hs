module Main where

import Data.Char (toUpper)
import Control.Monad

main = putStrLn "Write you string: " >> liftM shout getLine >>= putStrLn

shout = map toUpper	