import System.Environment
import Data.Char
import Control.Monad
-- main = getArgs >>= return . (!! 0) >>= readFile >>= writeFile "temp.txt"

--main = 
--  getArgs >>= 
--  return . (!! 0) >>= 
--  readFile >>= 
--  maximumPathSum 100 >>=
--  mWriteFile "temp.txt" 
main = do
  args <- getArgs 
  let fileName = args !! 1
      goalValue = (read $ args !! 0) :: Int
      resultFPath = args !! 2
  readFile fileName >>= maximumPathSum goalValue >>= mWriteFile resultFPath


type Index = Int
type Value = Int

maximumPathSum :: Int -> String -> IO [[(Index, Value)]]
maximumPathSum goalValue content = do
  let linesOfValue = lines content
  return $ maximumPathSum1 [] goalValue linesOfValue

type CurrentValue = Int
type Goal = Int
maximumPathSum1 :: [(Index, Value)] -> CurrentValue  -> [String] -> [[(Index, Value)]]
maximumPathSum1 candidateResult curValue contentList = 
  let lineData = zip [1..] (map (\x -> (read x :: Int)) (words . head $ contentList)) -- [(Index, Value)]
  in concat $ map (evalEach candidateResult curValue (tail contentList)) lineData
  where evalEach ::  [(Index, Value)] -> CurrentValue  -> [String] -> (Index, Value) -> [[(Index, Value)]]
        evalEach candidateResult curValue restContentList (index, value) = 
          let diff  = curValue - value 
          in  
            if diff < 0  
              then []
            else if diff > 0   
              then if not (null restContentList) 
                then maximumPathSum1 (candidateResult ++ [(index, value)]) diff restContentList 
                else [] 
            else if null restContentList
                 then [candidateResult ++ [(index, value)]]
                 else []

mWriteFile :: FilePath -> [[(Index, Value)]] -> IO ()
mWriteFile fPath listofList = 
  mapM_ (\each -> 
    mapM_ (\(index, value) -> 
      appendFile fPath $ "(" ++ show index ++ ", " ++ show value ++ ") " ) each >> 
      appendFile fPath "\n") 
  listofList

