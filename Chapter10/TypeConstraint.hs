data OrdStack a = Bottom
								| Item a (OrdStack a)
									deriving (Show)

isIncreasing :: Ord a => OrdStack a -> Bool
isIncreasing (Item e os@(Item innere _)) 
	| e < innere  = isIncreasing os
	| otherwise = False
isIncreasing _ = True
