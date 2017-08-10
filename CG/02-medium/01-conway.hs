import System.IO
import Control.Monad
import Data.List (intersperse)

runs :: [Int] -> [[Int]]
runs [] = [[]]
runs (x:xs) = [fst firstspan] ++ runs (snd firstspan)
    where firstspan = span (==x) (x:xs)

next :: [Int] -> [Int]
next r = concat [[length run, head run] | run <- init $ runs r]

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    [r, m] <- replicateM 2 $ (readLn :: IO Int)
    putStrLn $ concat $ intersperse " " $ map show $ (iterate next [r])!!(m - 1)
