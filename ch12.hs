

-- 1. Define an instance of the Functor class for the following type of binary
--trees that have data in their nodes:
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Functor Tree where
 -- fmap (a -> b) -> Tree a -> Tree b
  fmap g Leaf = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

{-
4. There may be more than one way to make a parameterised type into an
applicative functor. For example, the library Control.Applicative
provides an alternative ‘zippy’ instance for lists, in which the function
pure makes an infinite list of copies of its argument, and the operator <*>
applies each argument function to the corresponding argument value at
the same position. Complete the following declarations that implement
this idea: -}

newtype ZipList a = Z [a]
                    deriving Show

instance Functor ZipList where
  -- fmap (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) =  Z (fmap g xs)

{-
5.  Functor laws on page 156.
-}
{-
instance Functor [] where
-- fmap :: (a -> b) -> f a -> f b
fmap g [] = []
fmap g (x:xs) = fmap g xs ++ [g x]
-}

{-7.
Given the following type of expressions data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show that contain variables of some type a, show how to make this type into instances of the Functor, Applicative and Monad classes. With the aid of an example, explain what the >>= operator for this type does.
-}
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
              deriving Show

instance Functor Expr where
  -- fmap :: (a -> b) -> f a -> f b
  -- fmap :: (a -> b) -> Expr a -> Expr b
  fmap g (Var x) = Var (g x)
  fmap _ (Val x) = Val x
  fmap g (Add x y) = Add (fmap g x) (fmap g y)

