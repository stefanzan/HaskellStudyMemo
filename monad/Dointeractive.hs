nameDo :: IO ()
nameDo = do putStr "What is your first name?"
						first <- getLine
						putStr "What is your last name?"
						last <- getLine
						let full = fist ++ " " ++ last
						putStr $ "Pleased to meet you, " ++ full ++ "!"
						
nameLambda :: IO ()
nameLambda = putStr "What is your first name?" >>
						 getLine >>= \first ->
						 putStr "What is your last name?" >>
						 getLine >>= \last ->
						 let full = first ++ " " ++ last
						 in putStrLn $ "Pleased to meet you, " ++ full ++ "!"

nameReturn :: IO String
nameReturn = putStr "What is your first name?" >>
						 getLine >>= \first ->
						 putStr "What is your last name?" >>
						 getLine >>= \last ->
						 let full = first ++ " " ++ last
						 in putStrLn $ "Pleased to meet you, " ++ full ++ "!"
						    return full
greetAndSeeYou :: IO ()
greetAndSeeYou = do name <- nameReturn
							 	    putStrLn ("See you, " ++ name ++ "!")
										