leftSum :: Num a => [a] -> a
leftSum xs = foldl step 0 xs
    where step acc x = acc + x
