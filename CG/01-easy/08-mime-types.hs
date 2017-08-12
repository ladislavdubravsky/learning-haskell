import System.IO
import Control.Monad
import Data.Char (toLower)
import System.FilePath.Posix (takeExtension)

-- TOO SLOW
-- efficient hash table libraries not available for import
-- look into strictness in Haskell

unveil :: Maybe String -> String
unveil (Just s) = s
unveil Nothing  = "UNKNOWN"

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    [n, q] <- replicateM 2 (readLn :: IO Int)
    dic <- map ((\[x,y] -> ("." ++ map toLower x,y)) . words) <$> (replicateM n getLine)
    --hPrint stderr (lookup 2 [(1,1),(2,2),(3,3)])
    
    exts <- replicateM q $ takeExtension <$> getLine
    --hPrint stderr (exts)
    forM_ exts $ \ext -> do
        putStrLn $ unveil $ lookup ((map toLower) ext) dic
