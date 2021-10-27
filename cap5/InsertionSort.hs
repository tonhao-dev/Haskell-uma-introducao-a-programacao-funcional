insercao :: Ord a => [a] -> [a]
insercao = foldr insereOrd []

insereOrd ::(Ord a) => a -> [a] -> [a]
insereOrd x [] = [x]
insereOrd x (y:ys)
    | x <= y    = (x:y:ys)
    | otherwise = y: (insereOrd x ys)