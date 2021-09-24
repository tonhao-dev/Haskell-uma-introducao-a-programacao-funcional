absolute :: (Ord a, Num a) => a -> a
absolute n = if n < 0 then (-n) else n

signum :: (Ord a, Num a) => a -> a
signum n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1

quadratic :: (Floating a, Ord a) => a -> a -> a -> (a, a)
quadratic a b c = (x1, x2)
    where 
        x1 = if delta >= 0 then (-b + sqrt delta) / (2 * a) else 0
        x2 = if delta >= 0 then (-b - sqrt delta) / (2 * a) else 0
        delta = b ^ 2 - 4 * a * c