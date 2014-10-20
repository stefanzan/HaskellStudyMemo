-- http://book.realworldhaskell.org/read/code-case-study-parsing-a-binary-data-format.html

import Data.Int(Int64)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Word(Word8)
import Data.Char(chr)
import Control.Applicative

data ParseState = ParseState {
	string :: L.ByteString,
	offset :: Int64
} deriving(Show)

-- it consumes a parsing state, and produces both a new parsing state and some other pieces of information.

newtype Parse a = Parse {
	runParse :: ParseState -> Either String (a, ParseState)
}

-- identity parser
-- it is a parser generator, that, given a parameter "a", returns the actual parser.
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

-- parse accepts a parser and a bytestring, return an either.
parse :: Parse a -> L.ByteString -> Either String a
parse parser initialStr
	=  case runParse parser (ParseState initialStr 0) of
			Left err -> Left err
			Right (result, _) -> Right result

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset state newOffset =
	state { offset = newOffset}

parseByte :: Parse Word8
parseByte =
  getState ==> \initState ->
  case L.uncons (string initState) of
    Nothing -> 
        bail "no more input"
    Just (byte, remainder) ->
        putState newState ==> \_ -> identity byte
      where newState = initState { string = remainder, 
                                   offset = offset initState + 1 }

-- getState extracts the current parsing state.
getState :: Parse ParseState
getState = Parse (\state -> Right(state, state))

-- replace the current parsing state with a new one.
putState :: ParseState -> Parse ()
putState newState = Parse(\_ -> Right((), newState))

-- questin: the a here is really needed ?
bail :: String -> Parse a
bail err = Parse (\state -> Left $ 
					"byte offset " ++ show (offset state) ++ ": " ++ err)


(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
	where chainedParser initialState =
		case runParse firstParser initialState of
			Left errMessage ->
				Left errMessage
			Right (firstResult, newState) ->
				runParse (secondParser firstResult) newState

instance Functor Parse where
	fmap f parser = parser ==> \result -> 
									identity (f result)


w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte

-- peekByte is not so simple in fact.
peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState


peekChar :: Parse (Maybe Char)
peekChar =  fmap w2c <$> peekByte
-- fmap :: Maybe Word8 -> (Word8 -> Char) -> Maybe Char

-- generic combinator.
parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
               if mp == Just True
               then parseByte ==> \b ->
                 (b:) <$> parseWhile p
               else identity []

parseWhileVerbose p =
  peekByte ==> \mc ->
  case mc of 
    Nothing -> identity []
    Just c  | pc -> 
                parseByte ==> \b ->
                parseWhileVerbose p ==> \bs ->
                identity (b : bs)
            | otherwise -> 
                identity []



parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p =  fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits ->
           if null digits
           then bail "no more input"
           else let n = read digits
                in if n < 0
                   then bail "integer overflow"
                   else identity n
                

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> \_ f

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

assert :: Bool -> String -> Parse ()
assert True _ = identity ()
assert False err = bail err

parseBytes :: Int -> Parse L.ByteString
parseBytes n =
  getState ==> \st ->
  let n' = fromIntegral n
      (h, t) = L.splitAt n' (string st)
      st' = st { offset = offset + L.length h, string = t}
  in putState st' ==>&
     assert (L.length == n) "end of input" ==>&
     identity h


parseRawPGM = 
  parseWhileWith w2c notWhile ==> \header -> skipSpaces ==>&
  assert (header == "P5") "invalid raw header" ==>&
  parseNat ==> \width -> skipSpaces ==>&
  parseNat ==> \height -> skipSpaces ==>&
  parseNat ==> \maxGrey ->
  parseByte ==>&
  parseBytes (width * height) ==> \bitmap ->
  identity (GreyMap width height maxGrey bitmap)
 where notWhile = (`notElem` " \r\n\t")  
 