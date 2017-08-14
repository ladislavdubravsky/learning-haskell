import System.IO
import Control.Monad
import qualified Data.Map.Strict as M

buildNbrGraph [] g     = g
buildNbrGraph (e:es) g = buildNbrGraph es (addEdge e g)

addEdge [x,y] g = M.insert x (y:existingNbrs) g
    where existingNbrs = M.findWithDefault [] x g

deepest graph curDepth curNode
    | null nbrs = curDepth
    | otherwise = maximum $ map (deepest graph (curDepth+1)) nbrs
    where nbrs = M.findWithDefault [] curNode graph

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    n <- readLn :: IO Int
    edges <- replicateM n $ words <$> getLine
    let graph = buildNbrGraph edges M.empty
    print $ maximum $ map (deepest graph 1) (M.keys graph)
