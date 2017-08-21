import System.IO
import Control.Monad
import Data.List
import Data.List.Split

toDec []     = 0
toDec (x:xs) = x + 20*(toDec xs)

toMayan 0 = []
toMayan d = [mod d 20] ++ toMayan (div d 20)

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    [l, h] <- map (read :: String -> Int) . words <$> getLine
    
    numerals <- transpose <$> (replicateM h $ chunksOf l <$> getLine)
    let getDigit = (`elemIndices` numerals) <$> (replicateM h getLine)
    
    s1 <- readLn :: IO Int
    d1 <- toDec <$> reverse <$> concat <$> replicateM (div s1 h) getDigit
    
    s2 <- readLn :: IO Int
    d2 <- toDec <$> reverse <$> concat <$> replicateM (div s2 h) getDigit
    
    op <- getLine
    let res = reverse $ toMayan $ case op of "+" -> d1 + d2
                                             "-" -> d1 - d2
                                             "*" -> d1 * d2
                                             "/" -> d1 `div` d2

    if null res then mapM_ putStrLn (head numerals)
                else mapM_ putStrLn (concat $ map (numerals!!) res)
