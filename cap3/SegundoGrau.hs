quadratic :: Floating a => a -> a -> a -> (a, a)
quadratic a b c = (x1, x2)
    where 
        x1 = (-b + sqrt delta) / (2 * a)
        x2 = (-b - sqrt delta) / (2 * a)
        delta = b ^ 2 - 4 * a * c