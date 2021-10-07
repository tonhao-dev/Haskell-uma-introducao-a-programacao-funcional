imc :: Double -> Double -> Double
imc p a
    | valorImc <= 18.5 = "Abaixo do peso"
    | valorImc <= 25.0 = "Abaixo do peso"
    | valorImc <= 30 = "Abaixo do peso"
    | otherwise = "Abaixo do peso"
where
    valorImc = p / quadradoA
    quadradoA = a * a
