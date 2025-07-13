-- Define the custom List data type
data List a = Nil | Cons a (List a) deriving (Show, Eq)

-- Define length function for our custom List
myLength :: List a -> Integer
myLength Nil = 0
myLength (Cons x xs) = 1 + myLength xs

-- Define map function for our custom List
myMap :: (a -> b) -> List a -> List b
myMap f Nil = Nil
myMap f (Cons x xs) = Cons (f x) (myMap f xs)

-- Helper function to convert regular list to our List
fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

-- Helper function to convert our List to regular list
toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs