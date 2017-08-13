import System.IO
import Control.Monad

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    [w,h] <- map (read :: String -> Int) <$> words <$> getLine
    getLine
    [x,y] <- map (read :: String -> Int) <$> words <$> getLine
    loop x y 0 0 (w - 1) (h - 1)

loop x y xmin ymin xmax ymax = do
    bdir <- getLine
    let ymaxNew = if elem 'U' bdir then y - 1 else ymax
    let yminNew = if elem 'D' bdir then y + 1 else ymin
    let xminNew = if elem 'R' bdir then x + 1 else xmin
    let xmaxNew = if elem 'L' bdir then x - 1 else xmax
    let xNew = div (xminNew + xmaxNew) 2
    let yNew = div (yminNew + ymaxNew) 2
    putStrLn ((show xNew) ++ " " ++ (show yNew))
    loop xNew yNew xminNew yminNew xmaxNew ymaxNew
