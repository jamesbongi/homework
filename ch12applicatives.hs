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
  
instance Applicative ZipList where
-- pure :: a -> ZipList a
  pure x = Z $ repeat x
-- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  Z gs <*> Z xs = Z [g x | (g, x) <- zip gs xs]

{-
5.  Functor laws on page 156.
-}
{-
instance Functor [] where
-- fmap :: (a -> b) -> f a -> f b
fmap g [] = []
fmap g (x:xs) = fmap g xs ++ [g x]

instance Applicative [] where

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
  
instance Applicative Expr where
-- pure :: a -> Expr a
-- <*> :: Expr (a -> b) -> Expr a -> Expr b
  pure = Var
  _       <*> Val x = Val x
  Val x   <*> _ = Val x
  Var f   <*> Var x = Var (f x)
  Var f   <*> Add x y = Add (fmap f x) (fmap f y)
  Add f g <*> x = Add (f <*> x) (g <*> x)

