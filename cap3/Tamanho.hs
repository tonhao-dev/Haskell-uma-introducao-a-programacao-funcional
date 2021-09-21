size :: [a] -> Int
size [] = 0
size (_:xs) = 1 + size xs
