import System.IO
import Control.Monad
import Data.List

posibs = ["O(1)", "O(log n)", "O(n)", "O(n log n)", "O(n^2)", "O(n^2 log n)", "O(n^3)", "O(2^n)"]

corr :: [Double] -> [Double] -> Double
-- hack: if stuff is not correlated enough with another option, it's O(1)
corr xs ys = if dy == 0 then 0.7 else cxy / sqrt (dx * dy)
    where cxy    = sum $ zipWith (*) [x - avg xs | x <- xs] [y - avg ys | y <- ys]
          dx     = sum [(x - avg xs)**2 | x <- xs]
          dy     = sum [(y - avg ys)**2 | y <- ys]
          avg xs = (realToFrac $ sum xs) / (genericLength xs)

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    n <- readLn :: IO Int
    series <- replicateM n $ map (read :: String -> Double) . words <$> getLine
    let options = [map f (map head series) | f <- [const 1, log, id, \n -> n*log(n), (^2),
                                                   \n -> n^2*log(n), (^3), \n -> n^3*log(n), (1.01**)]]
    let corrs = map (corr (map last series)) options
    putStrLn $ posibs!!(head $ elemIndices (maximum corrs) corrs)
