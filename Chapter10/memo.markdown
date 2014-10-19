# Chaper 10
## Greyscale files
* PGM: portable grey map.

	* P2: ASCII

	* P5: raw binary
	
	* File content: header, format, white space, width, space,height, space, maximum grey value, image data.
	   
	* image data:

		P2: ASCII decimal numbers separated by single spce characters.
		
		P5: a string of binary values.

## Parsing a raw PGM file

###what I have learnt from code:

* some apis

     `import qualified Data.ByteString.Lazy.Char8 as L8`
 
     `L8.isPrefixOf:: ByteString -> ByteString -> Bool`
 
     `L8.dropWhile:: (Char -> Bool) -> ByteString -> ByteString`
 
     `L8.readInt :: ByteString -> Maybe (Int, ByteString)`

     `fromIntegral :: (Num b, Integral a) => a -> b`
 
     `L8.pack :: [Char] -> ByteString`
 
     `L8.unpack :: ByteString -> [Char]`
 
 
 
     `import qualified Data.ByteString.Lazy as L`
 
     `L.drop:: Int64 -> ByteString -> ByteString`
 
     `L.length::  ByteString -> Int64`
 
     `L.splitAt :: Int64 -> ByteString -> ByteString`
     
     `L.uncons :: ByteString -> Maybe (Word8, ByteString)`
 
 * some technicals
 
 	- funName parameters = case statement
 	    
 	- If the he result of case is Maybe type. For Just case, you can directly use condition. e.g.
 	
 	   Just (naxGrey, s4)
 	
        | maxGrey > 255   -> Nothing
      
        | otherwise -> case statemet  
        


## Getting rid of boilerplate code


some knowledge from code practice:

suppose we define a function:

`(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b`

` Nothing >>? _ = Nothing`

`Just v >>? f = f v`

we use >>? in another funciton:

`parseP5 :: L.ByteString -> Maybe (GreMap, L.ByteString)`

`parseP5 s = Just v >>? \s -> func`        
   
 Note: the definition of function parseP5, on the righ hand side of >>?, you shall read as an funciton which accpet an argument s and produce a new Maybe value.
 
 Previously, I always think what s it is: is s the Maybe type from previous one ? is s the wapped value v ? In fact, the argument s is related to the funciton defintion of >>?. As you can see: the type signature of >>?, the second parameter is a function. That is why we define a funciton. So s is the unwarpped value.
 
 
 
 
 
## Extra knowledge: newtype

* *data* keyword can only be replaced with *newtype* if the type has exactly one constructor with exactly one field insdie it.

* record syntax, with only one field is allowed.

  newtype State s a = State { runState :: s -> (s, a)}

`State :: (s -> (s,a)) -> State s a`

`runState :: State s a -> (s -> (s,a))`

in mathmatical terms, they are isomorhpic, which means at runtime the two types can be treated essentially the same.

Note: If you want to declare different type class instances for a particular type, or want to make a type abstract, you can wrap it in a newtype and it will be considered distinct to the **type-checker**, but identical at runtime.

## Implicit state and parser

      data ParseState = ParseState {
      	string :: L.ByteString,
      	offset :: Int64
      } deriving (Show)

A good parser shall return an Either type to including Left error or Right result. The Right result is a pair including the parsed informaiton a and the current state of parse which is called ParseState. The data structure of ParseState includes the remain string and the offset. We could add more information later.

	  newtype Parse a = Parse {
	  	runParse :: ParseState -> Either String (a, ParseState)
	  }	


`parse :: Parse a -> L.ByteString -> Either String a`

`parse parser initString = ...`

The function parse accept a *parser* and a bytestring, resulting in an either type.


## Operation on Record syntax

		modifyOffset :: ParseState -> Int64 -> ParseState
		modifyOffset currentState newOffset =
			currentState { offset = newOffset }
			
in fact, currentState is immutable. So the resultis a new ParseState.

**Note**: we can set many fields as we want inside the curly braces, separating them using commas.


## Hanging Lambdas

Advantage:

* make room for more text in the body of the function.

* make it more clear that there's a relationship between one function and the one that follows. For instance, the result of the first function is being passsed as a parameter to the second.

## Some Key Point

* The *Parse* type represents really a function inside a wrapper.


## My understanding of Functor

 Functor is nothing special, it is in fact just a Type Class. You can use any keyword to define your own functor.
 		
 		Class Functor f where
 			fmap :: (a -> b) -> f a -> f b
 
 The meaning of this Functor means: we define a type class **Functor**, where has parameter *f*. If you looking into the detail of funtion *fmap*, you would see some observations:
 	
 		* fmap is used to lift a from f a.
 		* applying a -> b to get b
 		* wap it again whith f. Get a f b.
 
 So, you can see another constraints: you can not get another g b. you can only get a new f b.		



			

  