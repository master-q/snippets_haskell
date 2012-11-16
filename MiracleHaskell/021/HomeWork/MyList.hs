{-# LANGUAGE TemplateHaskell #-}
module SomeModule where
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

-- My own List
infixr 5 :-:  
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

myMap :: (a -> b) -> List a -> List b
myMap _ Empty = Empty
myMap f (x :-: xs) = f x :-: myMap f xs

instance Functor List where
    fmap = myMap

fromList :: [a] -> List a
fromList [] = Empty
fromList (x:xs) = x :-: fromList xs

toList :: List a -> [a]
toList Empty = []
toList (x :-: xs) = x:toList xs

-- Testing
main :: IO ()
main = $(defaultMainGenerator)

prop_reverse :: [Int] -> Bool
prop_reverse xs = fmap f xs == (toList . fmap f . fromList $ xs)
  where
    f = (+ 1)
    types = xs::[Int]
