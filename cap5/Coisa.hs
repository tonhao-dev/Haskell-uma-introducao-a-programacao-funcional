data Coisa a = Nada | UmaCoisa a | DuasCoisas a a deriving Show

instance Eq a => Eq (Coisa a) where
    (DuasCoisas x1 y1) == (DuasCoisas x2 y2) = x1 == x2 && y1 == y2
    (UmaCoisa x) == (UmaCoisa y) = x == y
    Nada == Nada = True
    _ == _ = False