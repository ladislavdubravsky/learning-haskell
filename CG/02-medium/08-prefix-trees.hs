import System.IO
import Control.Monad
import qualified Data.Map.Strict as M

data Trie a = Trie (M.Map a (Trie a)) deriving Show

insert :: Ord a => [a] -> Trie a -> Trie a
insert [] trie            = trie
insert (c:cs) (Trie cmap) = Trie newmap
    where newmap = case M.lookup c cmap of
            Nothing -> M.insert c (insert cs (Trie M.empty)) cmap
            Just x  -> M.insert c (insert cs x) cmap

buildTrie :: Ord a => [[a]] -> Trie a -> Trie a
buildTrie [] trie     = trie
buildTrie (w:ws) trie = buildTrie ws (insert w trie)

countNodes :: Ord a => Trie a -> Int
countNodes (Trie cmap) | M.null cmap = 0
                       | otherwise   = (sum $ map countNodes (M.elems cmap))
                                       + length (M.elems cmap)

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    n <- readLn:: IO Int
    numbers <- replicateM n $ getLine
    print $ countNodes (buildTrie numbers (Trie M.empty))
