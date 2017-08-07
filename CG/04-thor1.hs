import System.IO

data Point = Point Int Int deriving (Show)

data Move = Move { point :: Point
                 , dir  :: String
                 } deriving (Show)

moveToLight :: Point -> Point -> Move
moveToLight (Point x y) (Point lx ly) = Move {point=Point u v, dir=dy ++ dx}
    where (u, dx) | x < lx    = (x + 1, "E")
                  | x > lx    = (x - 1, "W")
                  | otherwise = (x, "")
          (v, dy) | y < ly    = (y + 1, "S")
                  | y > ly    = (y - 1, "N")
                  | otherwise = (y, "")

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    [lx, ly, x, y] <- fmap (map (read :: String -> Int) . words) getLine
    loop (Point x y) (Point lx ly)

loop thorpos lightpos = do
    input_line <- getLine
    let remainingturns = read input_line :: Int
    let res = moveToLight thorpos lightpos
    putStrLn (dir res)
    loop (point res) lightpos
