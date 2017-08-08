import System.IO
import Data.Char (ord)

bin :: Int -> Int
bin 0 = 0
bin x = 10 * bin (div x 2) + mod x 2

padLeft :: Int -> String -> String
padLeft n s | n > length s = replicate (n - length s) '0' ++ s
            | otherwise    = s

runs :: Eq t => [t] -> [[t]]
runs [] = [[]]
runs (x:xs) = [fst firstspan] ++ runs (snd firstspan)
    where firstspan = span (==x) (x:xs)

chuckify :: String -> String
chuckify x = digit ++ " " ++ replicate (length x) '0' ++ " "
    where digit | x !! 0 == '0' = "00"
                | otherwise     = "0"

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    message <- getLine
    let step1 = concat $ map ((padLeft 7) . show . bin . ord) message
    let step2 = concat $ (map chuckify) $ init $ runs step1
    putStrLn $ init step2
