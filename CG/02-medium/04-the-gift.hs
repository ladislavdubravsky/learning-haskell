import System.IO
import Control.Monad
import Data.List (sort)

calcContribs :: [Int] -> Int -> [Int]
calcContribs (budget:budgets) price
    | length budgets == 0 = [price]
    | budget < fair       = budget:calcContribs budgets (price - budget)
    | otherwise           = fair:calcContribs budgets (price - fair)
    where fair = div price (1 + length budgets)

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE    
    [n,price] <- replicateM 2 $ (readLn :: IO Int)
    budgets <- sort <$> (replicateM n $ (readLn :: IO Int))
    
    let contribs = calcContribs budgets price
    hPrint stderr (price, budgets)
    
    do (if price > sum budgets
        then putStrLn "IMPOSSIBLE"
        else forM_ contribs $ \x -> putStrLn $ show x)
