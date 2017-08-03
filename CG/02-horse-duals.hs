import System.IO
import Control.Monad
import Data.List

mindif :: (Ord a, Num a) => [a] -> a
mindif []       = 999999
mindif (_:[])   = 999999
mindif (x:y:zs) = min (abs $ x - y) (mindif (y:zs))

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    n <- readLn :: IO Int
    -- note: getLine :: IO String
    --       readLn :: Read a => IO a
    ps <- sequence $ replicate n (readLn :: IO Int)

    print $ mindif $ sort ps
