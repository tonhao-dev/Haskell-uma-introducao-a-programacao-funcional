-- is equivalent to data Point = Coordinate Double Double
data Point = Coordinate {x, y :: Double} deriving Show

distanceToOrigin :: Point -> Double
distanceToOrigin (Coordinate x y) = sqrt (x**2 + y**2)