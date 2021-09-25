-- 3.1)
data Question = Yes | No deriving Show

questionNumber :: Question -> Int
questionNumber Yes = 1
questionNumber No  = 0

questionList:: [Question]-> [Int]
questionList [] = []
questionList (x:xs) = questionNumber x : questionList xs 

and' :: Question -> Question -> Question
and' Yes Yes = Yes
and' _ _ = No

or' :: Question -> Question -> Question
or' No No = No
or' _ _ = Yes

not' :: Question -> Question
not' Yes = No
not' No = Yes

-- 3.2)
data Temperature = Celsius | Fahrenheit | Kelvin

convertCelsius :: Double -> Temperature -> Double
convertCelsius f Fahrenheit = (f - 32) / 1.8
convertCelsius k Kelvin = k - 273

convertKelvin :: Double -> Temperature -> Double
convertKelvin c Celsius = c + 273
convertKelvin f Fahrenheit = (f - 32) * 5 / 9 + 273

convertFahrenheit :: Double -> Temperature -> Double
convertFahrenheit c Celsius = c * 1.8 + 32
convertFahrenheit k Kelvin = (k - 273) * 1.8 + 32

-- 3.3)
data GameOption = Paper | Scissor | Rock deriving Show

gameWinner:: GameOption -> GameOption -> GameOption
gameWinner Paper Scissor = Scissor
gameWinner Paper Rock = Paper
gameWinner Scissor Rock = Rock
gameWinner x y = x