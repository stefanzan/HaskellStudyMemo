--{-# LANGUAGE ConstraintKinds #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xt@(x:xs) yt@(y:ys) 
	| x <= y = x : (merge xs yt)
	| x > y =  y : (merge xt ys)
    