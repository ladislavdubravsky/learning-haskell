import System.IO
import Control.Monad
import Data.List.Split (splitOn)
import Data.List (elemIndices)

repl     [] = []
repl (x:xs) = if x == ',' then '.':repl xs else x:repl xs

toRad = (pi/180*) . (read :: String -> Float) . repl

distXY [x0,y0] [x,y] = 6371 * sqrt(u^2 + v^2)
    where u = (x - x0) * cos(0.5 * y0 + 0.5 * y)
          v = y0 - y

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    [lon, lat] <- replicateM 2 $ toRad <$> getLine
    n <-  readLn :: IO Int

    defibs <- replicateM n $ (splitOn ";") <$> getLine
    let coords = map (map toRad . take 2 . drop 4) defibs
    let dists = map (distXY [lon, lat]) coords
    let indMin = (elemIndices (minimum dists) dists) !! 0
    putStrLn (defibs !! indMin !! 1)
