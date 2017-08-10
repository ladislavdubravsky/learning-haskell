import System.IO
import Control.Monad
import Data.List

mindif :: (Bounded a, Ord a, Num a) => [a] -> a
mindif []       = maxBound
mindif (_:[])   = maxBound
mindif (x:y:zs) = min (abs $ x - y) (mindif (y:zs))

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    n <- readLn :: IO Int
    -- note: getLine :: IO String
    --       readLn :: Read a => IO a
    ps <- sequence $ replicate n (readLn :: IO Int)

    print $ mindif $ sort ps
