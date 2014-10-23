import Control.Monad

digit :: Int -> String -> Maybe Int
digit i s 
  | i > 9 || i < 0    = Nothing
  | otherwise         = do
    let (c : _) = s
    if [c] == show i then Just i else Nothing

-- Note digit is just a basic function, which is used for bigger parser.    

binChar :: String -> Maybe Int
binChar s = digit 0 s `mplus` digit 1 s

-- I want: String -> (String -> Maybe Int) -> (String -> Maybe Int) -> Maybe Int

binCharFree :: String -> Maybe Int
binCharFree  = \s -> digit 0 s `mplus` digit 1 s