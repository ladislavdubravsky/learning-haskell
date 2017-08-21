import System.IO
import Control.Monad
import qualified Data.Map.Strict as M

type Node  = Int
type Graph = M.Map Node [Node]

farthest :: Graph -> Node -> (Int, Node)
farthest g n = farthest' g 0 [] n

farthest' :: Graph -> Int -> [Node] -> Node -> (Node, Int)
farthest' g dist visited currnode
    | null unvisitedNbrs = (dist, currnode)
    | otherwise          = maximum [farthest' g (dist+1) (currnode:visited) nextnode |
                                    nextnode <- unvisitedNbrs]
    where unvisitedNbrs = filter (\x -> not $ elem x visited) $ (g)M.!currnode

-- from edgelist to nbr rep
addEdge g [x,y] = M.insertWith (++) y [x] (M.insertWith (++) x [y] g)

myfold f x []     = x
myfold f x (y:ys) = myfold f (f x y) ys

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    n <- readLn :: IO Int
    edges <- replicateM n $ (map (read :: String -> Int) . words) <$> getLine
    let graph = myfold addEdge M.empty edges
    print $ div (1 + fst (farthest graph (snd $ farthest graph 1))) 2
