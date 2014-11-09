# Control.Monad.Writer

## The Writer type

     newtype Writer w a = Writer { runWriter:: (a, w)}

The Monad instance of Writer:

      instance (Moniod w) => Monad (Writer w) where
        return x = Writer (x, mempty)
        (Writer (x, v)) >>= f = let Writer (y, v') = f x
                                in Writer (y, v `mappend` v')


* mempty is used to present identity monoid values.
   such as "" and Sum 0, and empty byteStrings.

* When using the Writer monad, you have to be careful which monoid to use, because using lists can sometimes
turn out to be very slow.


#### Difference Lists

     [1,2,3] = \xs -> [1,2,3] ++ xs
     []      = \xs -> [] + xs
  And it supports efficient appending.


      newtype DiffList a = DiffList { getDiffList :: [a] -> [a]}

      toDiffList :: [a] -> DiffList a
      toDiffList xs = DiffList (xs++)

      fromDiffList :: DiffList a -> [a]
      fromDiffList (DiffList f) = f []
A difference list is a function that prepends something
to another list.


      instance Monoid (DiffList a) where
          mempty = DiffList (\xs -> [] ++ xs)
          (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))
                
