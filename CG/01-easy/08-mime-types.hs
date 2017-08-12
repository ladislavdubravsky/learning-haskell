import System.IO
import Control.Monad
import Data.Char (toLower)
import System.FilePath.Posix (takeExtension)
import qualified Data.Map as M

-- OK speed to pass tests now
-- but still need to research efficient hash table libraries and strictness

unveil :: Maybe String -> String
unveil (Just s) = s
unveil Nothing  = "UNKNOWN"

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    [n, q] <- replicateM 2 (readLn :: IO Int)
    dic <- M.fromList <$> map ((\[x,y] -> ("." ++ map toLower x,y)) . words) <$> (replicateM n getLine)
    exts <- replicateM q $ takeExtension <$> getLine
    forM_ exts $ \ext -> do
        putStrLn $ unveil $ M.lookup ((map toLower) ext) dic
