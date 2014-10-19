import Data.Int(Int64)
import qualified Data.ByteString.Lazy as L
import Date.Word(Word8)


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
			putState newState ==> \_ ->
			identity byte
		where newState = initState { string = remainder,
																	offset = newOffset
																}
					newOffset = offset initState + 1

-- getState extracts the current parsing state.
getState :: Parse ParseState
getState = Parse (\state -> Right(state, state))

-- replace the current parsing state with a new one.
putState :: ParseState -> Parse ()
putState newState = Parse(\_ -> Right(_, newState))

-- questin: the a here is really needed ?
bail :: String -> Parse a
bail err = Parse (\state -> Left $ 
					"byte offset " ++ show (offset state) ++ ": " ++ err)


(==>) :: Parse a -> (a -> Parse b) -> Parse b
(==>) firstParser ==> secondParser = Parse chainedParser
	where chainedParser initialState =
		case runParse firstParser initialState of
			Left errMessage ->
				Left errMessage
			Right (firstResult, newState) ->
				runParse (secondParser firstResult) newState

