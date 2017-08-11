import System.IO
import Control.Monad

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    [n,l,e] <- map (read :: String -> Int) . words <$> getLine    
    links <- replicateM l $ map (read :: String -> Int) . words <$> getLine
    gates <- replicateM e $ (read :: String -> Int) <$> getLine
    loop links gates

loop links gates = do
    si <- readLn :: IO Int
    let test = map (\l -> (l, elem si l && (elem (l!!0) gates || elem (l!!1) gates))) links
    let link = unwords $ map show $ fst $ head $ (filter snd test) ++ [head test]    
    putStrLn link
    loop links gates
