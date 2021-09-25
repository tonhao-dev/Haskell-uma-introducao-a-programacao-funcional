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