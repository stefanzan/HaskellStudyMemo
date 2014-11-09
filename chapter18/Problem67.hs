{-By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

		3
	   7  4
	 2  4  6
	8  5  9  3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom in triangle.txt (right click and 'Save Link/Target As...'), a 15K text file containing a triangle with one-hundred rows.

NOTE: This is a much more difficult version of Problem 18. It is not possible to try every route to solve this problem, as there are 299 altogether! If you could check one trillion (1012) routes every second it would take over twenty billion years to check them all. There is an efficient algorithm to solve it. ;o)
-}

module Main where
 
acc_l [] (o1:[]) = []
acc_l (c1:cs) (o1:o2:os) = ((max o1 o2) + c1) : acc_l cs (o2:os)
 
main = do 
    lns <- readFile "triangle1.txt"
 
    let real_lns = map (\l -> [ read x::Int | x <- words l ]) (lines lns)
 
    --Haskell里头列表项的值是不可变的，用foldr做累加。
    print $ head $ foldr (\nl ol-> acc_l nl ol) (last real_lns) (init real_lns)
 
    return ()