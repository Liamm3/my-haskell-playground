-- Chapter 4

import System.Environment (getArgs)
import Data.Char          (digitToInt, isSpace)
import Data.List          (groupBy)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            _               -> putStrLn "error: exactly two arguments are needed"
        myFunction = id

asInt_fold :: String -> Int
asInt_fold []       = 0
asInt_fold ('-':xs) = (-1) * asInt_fold xs
asInt_fold xs       = foldl step 0 xs
    where step acc x = acc * 10 + digitToInt x


concat' :: [[a]] -> [a]
concat' = foldr (++) []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' p ls = foldr step [] ls
    where step acc ys
              | p acc     = acc : ys
              | otherwise = []

smaller :: (Ord a) => a -> a -> Bool
smaller a b = a < b

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ []    = []
