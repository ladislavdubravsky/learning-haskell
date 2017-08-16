import System.IO
import Control.Monad
import Data.List (sort)

type Task = (Int,Int)

processInp :: String -> Task
processInp = (\[x,y] -> (x + y,x)) . map (read :: String -> Int) . words

cntTasks :: [Task] -> Int -> Int
cntTasks [] _              = 0
cntTasks (t:ts) minEndTime = if snd t >= minEndTime
                             then 1 + cntTasks ts (fst t)
                             else cntTasks ts minEndTime

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    n <- readLn :: IO Int
    tasks <- sort <$> (replicateM n $ fmap processInp getLine)
    print $ cntTasks tasks 0
