selecao :: (Ord a) => [a] -> [a]
selecao []  = []
selecao xs  = [x] ++ selecao (remove x xs)
    where x = minimo xs

remove :: (Ord a) => a -> [a] -> [a]
remove value [] = []
remove value (x:xs)
    | value == x = xs
    | otherwise  = x : (remove value xs)

minimo :: (Ord a) => [a] -> a
minimo [x] = x
minimo (x:xs)
    | x <= (minimo xs) = x
    | otherwise        = minimo xs