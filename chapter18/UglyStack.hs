import System.Directory
import System.FilePath
import Control.Monad.Reader
import Control.Monad.State
import CountEntries (listDirectory)

data AppConfig = AppConfig {
  cfgMaxDepth :: Int
} deriving (Show)

data AppState = AppState {
  stDeepestReached :: Int
} deriving (Show)

type App = ReaderT AppConfig (StateT AppState IO)

-- type App2 a = ReaderT AppConfig (StateT AppState IO) a
-- ReaderT  r m a
-- StateT   s m a

-- Since Haskell does not allow us to partially apply a type synonym. So you
-- can not build: WriterT [String] App2 a; But for WriterT [String] App a. it is ok.
-- or wrap it with newtype.

runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth =
  let config = AppConfig maxDepth -- set the configuration
      state = AppState 0 -- initial state
  in runStateT (runReaderT k config) state

-- Explanation: runReaderT :: ReaderT r m a -> r -> m a.
-- runStateT :: StateT s m a -> s -> m (a, s)
-- so the final result is : IO (a, s)

-- In one word, runReaderT and runStateT removes the transformer wrapper.

constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]  
constrainedCount curDepth path = do
  contents <-  liftIO . listDirectory $ path
  cfg <- ask -- where shall we input this configuration.
  --st <- get  -- it is possible to use get here.
  --put st { stDeepestReached = 20}
  rest <- forM contents $ \name -> do
    let newPath = path </> name
    isDir <-  liftIO . doesDirectoryExist $ newPath
    if isDir && curDepth < cfgMaxDepth cfg
      then do
        let newDepth = curDepth + 1
        st <- get
        when (stDeepestReached st < newDepth) $ 
          put st { stDeepestReached = newDepth }
        constrainedCount newDepth newPath
      else
        return []
  return $ (path, length contents): concat rest


newtype MyApp a = MyA {
  runA :: ReaderT AppConfig (StateT AppState IO) a
} deriving (Monad, MonadIO, MonadReader AppConfig, MonadState AppState)

runMyApp :: MyApp a -> Int -> IO (a, AppState)
runMyApp k maxDepth =
  let config = AppConfig maxDepth
      state =  AppState 0
  in runStateT (runReaderT (runA k) config) state  
  