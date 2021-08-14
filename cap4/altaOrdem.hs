dobro :: Int -> Int
dobro x = 2*x

tripo :: Int -> Int
tripo x = 3*x

calculaESomaUm :: (Int -> Int) -> Int -> Int
calculaESomaUm funcao x = funcao x + 1

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)