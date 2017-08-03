import System.IO
import Control.Monad
import Data.List

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    input <- sequence $ replicate 8 getLine    
    print $ snd $ last $ sort (zip input [0..])
    main
