# CoreToLenses.hs

## Basic definitions

* ProcArgs: arguments of procedure

		type ProcArgs = (DynType, DynType, [(Var, DynType)])

	Source type, view type and environment variables.

* ProcsArgs:

		type ProcsArgs = (EnvT, ViewVars s)

	variables in the lens environment and current variables from the original source as lens.

	**Q**:

	* 1. what is *EnvT* ?


* ViewVars

		type ViewVars s = Map String (EnvViewPf s)

* EnvView

		type EnvView s = View (EnvM) s
		type EnvViewPf s = ViewPf (EnvM) s

Source location: View.hs, ViewPf.hs, Pf.hs

     type EnvM = (State Env) -- Lib.hs

		 type Env = Map String Dynamic -- Type.hs
		 type EnvT = Map String DynType -- Type.hs

		 data Dynamic where
				Dyn :: Eq a => Type a -> a -> Dynamic  -- Type.hs

* EnvRef

		type EnvRef v = Ref ((State Env)) v
		type EnvRefPf v = RefPf ((State Env)) v

* quite hard

  to be continued

## Program Part

### evalcprogram

		evalcprogram :: CProgram -> TypeEnv -> Type s -> Type v -> m (Lens s v)

evaluate the core program into lens.

	  evalcprogram program tenv s v = liftM (put2lens . runStatePut (\s v -> return Map.empty)) (evalcprogramput program tenv s v)

1. why runStatePut return Map.empty ?

2. what is s, v in \s v -> return Map.empty ?

3. haven't understand what this function do ?

   I think it is because *evalcprogramput* is m (EnvPutlensM s v), while the expected result is m (Lens s v). So a lift is needed.

Knowledge: Control.Monad:

		liftM :: Monad m => (a1 -> r) -> m a1 -> m r

Promote a function to a monad.


### evalcprogramput

		evalcprogramput :: CProgram -> TypeEnv -> Type s -> Type v -> m (EnvPutlensM s v)

		evalcprogramput prog tenv s v = cprogram2lens prog tenv s v >>= \(PFWithDecls lns devls) -> return $ evalPutlnsM decls envM s v lns

1. what is PfWithDecls ?
2. what is evalPutlnsM ?

### cprogram2lens

		cprogram2lens :: CProgram -> TypeEnv -> Type s -> Type v -> m (PfWithDecls (EnvPutlensM s v))

1. what is EnvPutlensM ?

			cprogram2lens (CProgram (CStart name sargs vargs eargs) procedures) tenv s v = do
				let procsargs = foldr (\(CProcedure n s v e stmt) m -> Map.insert n (s,v,e) m) Map.empty procedures
				    prevs = ([], Map.empty)
					envs = (Map.empty, Map.empty)
					m = mapM (cprocedure2lens procsargs) procedures >> start2lens name sargs vargs eargs s v
				(lns, (_, decls)) <- State.evalStateT (Reader.runReaderT (State.runStateT m prevs) tenv) (envs, procargs)
				return $ PfWithDecls lns decls

(1). State.evalStateT ?

(2). Reader.runReaderT ?

(3). State.runStateT ?

Knowledge: Data.List

		foldr :: (a -> b -> b) -> b -> [a] -> b

applied to a binary operator, a starting value (typically the right-identity of the operator), and a list, reduces the list using the binary operator, from right to left.

Data.Map:

		insert :: Ord k => k -> a -> Map k a -> Map k a  

Control.Monad:

		mapM :: Monad m => (a -> m b) -> [a] -> m [b]

mapM f is equivalent to sequence . map f.


### start2lens

### cprocedure2lens

### cprocedureStmt2lens


### cstmt2lens

start from this is the core part, every statement to lens.
