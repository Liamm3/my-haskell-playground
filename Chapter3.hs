-- Exercises Chapter 3
import Data.List (sortBy)

-- 1, 2
myLength :: [a] -> Int
myLength (_:xs) = 1 + myLength xs
myLength []     = 0

-- 3
mean :: (Fractional a) => [a] -> a
mean xs = sum xs / fromIntegral (length xs)

-- 4
palindrome :: [a] -> [a]
palindrome xs = xs ++ reverse xs

-- 5
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- 6
sortList :: [[a]] -> [[a]]
sortList xs             = sortBy compareList xs
  where compareList a b = compare (length a) (length b)

-- 7
myIntersperse :: a -> [[a]] -> [a]
myIntersperse _   []     = []
myIntersperse _   [x]    = x
myIntersperse sep (x:xs) = x ++ [sep] ++ myIntersperse sep xs

-- 8
data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

myTree :: Tree String
myTree = Node "parent" (Node "child" Empty Empty) (Node "child" Empty Empty)

height :: Tree a -> Int
height Empty               = 0
height (Node _ left right) = 1 + max (height left) (height right)

-- 9, 10
data Point = Point Double Double
           deriving (Show, Eq)
data Direction = DLeft | DRight | DStraight
               deriving (Show)

calcDirection :: Point -> Point -> Point -> Direction
calcDirection (Point x1 y1) (Point x2 y2) (Point x3 y3)
  | result < 0  = DRight
  | result > 0  = DLeft
  | result == 0 = DStraight
  where result = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

-- 11
turns :: [Point] -> [Direction]
turns (a:b:c:rest) = (calcDirection a b c):(turns (b:c:rest))
turns _            = []
