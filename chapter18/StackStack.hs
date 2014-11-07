type Foo = StateT Int (State String)

outerPut :: Int -> Foo ()
outerPut = put 

-- | access inner State
innerPut :: String -> Foo ()
innerPut = lift . put 

type Bar = ReaderT Bool Foo

-- | access more than one level down the stack
barPut :: String -> Bar ()
barPut =  lift . lift . put

