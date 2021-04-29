{-# LANGUAGE InstanceSigs #-}

-- | Contains instances for NonEmpty list
module Block1.Task3
  ( NonEmpty (..),
  )
where

-- | Non empty list realization
data NonEmpty a = a :| [a] 
  deriving (Show, Eq)

instance Functor NonEmpty where
  fmap f (x1 :| xs) = f x1 :| fmap f xs

instance Applicative NonEmpty where
  pure x = x :| []

  (<*>) (f :| fs) (x :| xs) = f x :| (map f xs ++ (fs <*> xs))

instance Foldable NonEmpty where
  foldr f x0 (x1 :| xs) = f x1 (foldr f x0 xs)
  
instance Traversable NonEmpty where
  traverse f (x1 :| xs) = (:|) <$> f x1 <*> traverse f xs

instance Monad NonEmpty where
  return x = x :| []

  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (>>=) (x :| []) f = f x 
  (>>=) (h :| (x : xs)) f = join (f h) ((x :| xs) >>= f)
    where
    join (x :| xs) (y :| ys) = x :| (xs ++ y : ys)