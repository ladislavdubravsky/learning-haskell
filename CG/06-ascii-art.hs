import System.IO
import Control.Monad
import Data.Char (ord)

getInd :: Char -> Int
getInd c | ord c >= 65 && ord c <= 90  = ord c - 65
         | ord c >= 97 && ord c <= 122 = ord c - 97
         | otherwise                   = 26

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    [width, height] <- replicateM 2 $ (read :: String -> Int) <$> getLine
    inds <- (map getInd) <$> getLine
    
    replicateM height $ do
        row <- getLine
        let res = map (\ind -> take width . drop (ind * width) $ row) inds
        putStrLn (concat res)
