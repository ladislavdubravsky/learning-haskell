import System.IO

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE    
    getLine
    nums <- map (read :: String -> Int) . words <$> getLine
    print $ minimum $ zipWith (\x y -> y - x) (scanl1 max nums) (scanr1 min nums)
