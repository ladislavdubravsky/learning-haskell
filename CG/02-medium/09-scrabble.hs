import System.IO
import Control.Monad
import Data.Char
import Data.List
import Data.Function (on)
import qualified Data.Map.Strict as M

price :: String -> Maybe Int
price w = sum <$> sequence (map (\c -> M.lookup c prices) w)

canBeMadeOf :: [Char] -> String -> Bool
canBeMadeOf _ []           = True
canBeMadeOf [] _           = False
canBeMadeOf letters (x:xs) = if elem x letters
                             then canBeMadeOf (letters \\ [x]) xs
                             else False

prices = M.fromList[
    ('e',1),('a',1),('i',1),('o',1),('n',1),('r',1),('t',1),('l',1),('s',1),('u',1),
    ('d',2),('g',2),('b',3),('c',3),('m',3),('p',3),('f',4),('h',4),('v',4),('w',4),('y',4),
    ('k',5),('j',8),('x',8),('q',10),('z',10)]

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    n <- readLn :: IO Int
    wrds <- replicateM n $ getLine
    letters <- getLine
    let possible = filter (canBeMadeOf letters) wrds
    putStrLn (maximumBy (on compare price) (reverse possible))
