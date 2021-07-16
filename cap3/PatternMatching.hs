f :: (Int, Int) -> Int
f (0, 0) = 0
f (0, 1) = 1
f (1, 0) = 1
f (x, 0) = x
f (0, y) = y
f (x, y) = x+y

g :: (Int, Int) -> Int
g (7, 7) = 7
g (_, _) = 0

h :: [Int] -> Int
h [] = 0
h (_:[]) = 1
h (_:x:[]) = 2+x