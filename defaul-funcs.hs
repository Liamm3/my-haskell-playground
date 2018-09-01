head' :: [a] -> a
head' []    = error "empty list"
head' [x]   = x
head' (x:_) = x

tail' :: [a] -> [a]
tail' []     = error "empty list"
tail' [x]    = []
tail' (_:xs) = xs

take' :: Int -> [a] -> [a]
take' _ []     = []
take' 0 _      = []
take' n (x:xs) = x:take' (n - 1) xs

drop' :: Int -> [a] -> [a]
drop' _ []     = []
drop' 0 xs     = xs
drop' n (_:xs) = drop' (n - 1) xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _    []     = []
takeWhile' pred (x:xs)
    | pred x    = x : takeWhile' pred xs
    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _    []         = []
dropWhile' pred xss@(x:xs)
    | pred x    = dropWhile' pred xs
    | otherwise = xss

zip' :: [a] -> [b] -> [(a, b)]
zip' _      []     = []
zip' []     _      = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ []     _      = []
zipWith' _  _     []     = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

square :: Num a => a -> a
square x = x * x

filter' :: (a -> Bool) -> [a] -> [a]
filter' _    []     = []
filter' pred (x:xs)
    | pred x    = x : filter' pred xs
    | otherwise = filter' pred xs

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
