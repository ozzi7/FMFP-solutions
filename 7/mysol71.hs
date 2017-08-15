data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)

labelInOrder :: Tree a -> Tree Int
labelInOrder 