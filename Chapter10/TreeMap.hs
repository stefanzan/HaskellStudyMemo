data Tree a = Node (Tree a) (Tree a)
						| Leaf a
							deriving (Show)

treeLengths (Leaf s) = Leaf (length s)
treeLengths (Node l r) = Node (treeLengths l) (treeLengths r)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf leaf) = Leaf (f leaf)
treeMap f (Node l r)  = Node (treeMap f l) (treeMap f r)

class MFunctor f where
	fmap :: (a -> b) -> f a -> f b

instance MFunctor Tree where
	fmap = treeMap

data Foo a = Foo a

instance Functor Foo where
	fmap f (Foo a) = Foo (f a)


