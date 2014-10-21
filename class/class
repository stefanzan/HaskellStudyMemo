# Haskell/Classes
## Classes and instances

    class Eq a where
      (==), (/=) :: a -> a -> Bool

      x /= y   = not (x == y)
      x == y   = not (x /= y)
If a type *a* is to be an instance of class Eq, it needs support the functions (==) and (/==).

#### Marks
* Classes are not types, but categories for types. The instances of a class are types instead of values.
* Type synonyms defined with *type* keyword cannot be made instances of a class.



## Class inheritance

    class (Eq a) => Ord a  where
        compare  :: a -> a -> Ordering
        (<), (<=), (>=), (>) :: a -> a -> Bool
        max, min :: a -> a -> a

This means Ord inherits from Eq, which is indicated by the => ntation.
Hence needs to implement the == and /= operations.

## Star¥ndard classes

![](http://upload.wikimedia.org/wikipedia/commons/thumb/6/69/Classes.svg/480px-Classes.svg.png)

## Type constraints on functions

    (+) :: (Num a) => a -> a -> a

## What I have learnt from Code From an [example](http://en.wikibooks.org/wiki/Haskell/Classes_and_types) ?

1. typeclasses are abstract, or like generic template in C++. You specify the behavior of a list of things.
Finally use **instance .. where** to declare an instance type of this type class. In this declaration, you have to implement the functions defined in this typeclass corresponded to the instantiated type.

2. typeclass itself can inheriated from each other.

        class C1 a where
          .....

        class (C1 a) => C2 b where
          .....

C2 inheriates all the functions from C1.

3. Functions are in fact not attached with some specified ad-hoc types, but you can, and sometimes you only can.
But you may write general funcitons Like the move.

        move :: (Movable a) => (Int, Int) -> a -> a

So, the move function says you could move something with (Int, Int). But something is a generic type a.
Later you could specify any types that satisfy **Movable** typeclass.

This is the only constraint.
