import System.IO
import Control.Monad
import Data.List

medianf x = if odd n
            then sort x !! (div n 2)
            else div (sort x !! (div n 2 - 1) + sort x !! (div n 2)) 2
            where n = length x

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    n <- readLn :: IO Int
    [xs,ys] <- transpose <$> (replicateM n $ map (read :: String -> Int) <$> words <$> getLine)
    let med = medianf ys
    print $ sum [abs (y - med) | y <- ys] + maximum xs - minimum xs
