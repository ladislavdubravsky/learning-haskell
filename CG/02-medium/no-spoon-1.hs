import System.IO
import Control.Monad

processNode :: String -> Int -> Int -> Int -> IO ()
processNode field w h coord
    | node == '.' = return ()
    | otherwise   = printMe >> printRight >> printBelow
    where node = field !! coord
          printMe = putStr $ (show $ mod coord w) ++ " " ++ (show $ div coord w) ++ " "
          printRight = putStr $ (show $ fst $ getRight field w h coord True) ++ " "
                             ++ (show $ snd $ getRight field w h coord True) ++ " "
          printBelow = putStrLn $ (show $ fst $ getBelow field w h coord True) ++ " "
                               ++ (show $ snd $ getBelow field w h coord True) ++ " "

getRight field w h coord first
    | first && mod coord w == w - 1       = (-1,-1)
    | mod coord w == w - 1 && node == '.' = (-1,-1)
    | node /= '.' && not first            = (mod coord w, div coord w)
    | otherwise                           = getRight field w h (coord + 1) False
    where node = field !! coord

getBelow field w h coord first
    | first && div coord w == h - 1       = (-1,-1)
    | div coord w == h - 1 && node == '.' = (-1,-1)
    | node /= '.' && not first            = (mod coord w, div coord w)
    | otherwise                           = getBelow field w h (coord + w) False
    where node = field !! coord

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    width <- readLn :: IO Int
    height <- readLn :: IO Int
    field <- concat <$> (replicateM height getLine)
    mapM_ (processNode field width height) [0..(width*height - 1)]
