module Logger
  (
    Logger,
    Log,
    runLogger,
    record
  ) where

type Log = [String]

runLogger :: Logger a -> (a, Log)

record :: String -> Logger ()

