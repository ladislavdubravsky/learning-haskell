import System.IO

minAbs :: [Int] -> Int
minAbs []     = 0
minAbs (x:[]) = x
minAbs (x:xs) | abs x < abs rest = x
              | abs x > abs rest = rest
              | x == rest        = x
              | otherwise        = abs x
              where rest = minAbs xs

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    n <- readLn :: IO Int
    temps <- getLine    
    print $ minAbs $ map (\x -> read x :: Int) (words temps)
