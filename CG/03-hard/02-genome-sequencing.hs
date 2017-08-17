import System.IO
import Control.Monad
import Data.List

joinw :: String -> String -> String
joinw w1 w2
    | isInfixOf w1 w2 = w2
    | isInfixOf w2 w1 = w1
    | otherwise       = tryJoin w1 w2 (min (length w1) (length w2))
    where tryJoin w1 w2 0 = w1 ++ w2
          tryJoin w1 w2 n
              | takeEnd n w1 == take n w2 = w1 ++ (drop n w2)
              | otherwise                 = tryJoin w1 w2 (n - 1)

takeEnd :: Int -> String -> String
takeEnd n lst = drop (length lst - n) lst

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    n <- readLn :: IO Int
    wrds <- replicateM n $ getLine
    print $ minimum $ map (length . foldl1' joinw) (permutations wrds)
