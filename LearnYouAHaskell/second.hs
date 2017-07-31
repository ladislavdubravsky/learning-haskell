-- ghci
-- :l C:\Haskell\LearnYouAHaskell\second.hs
-- :r reload after source change

-- Python's from x import *, otherwise use import qualified Data.List as DL
import Data.List

main = putStrLn "Stuff 2"

-- DATA.LIST
-- reimplement some funcs as excercise
-- iterate
iterate2 :: (a -> a) -> a -> [a]
iterate2 f x = (f x):(iterate2 f (f x))
-- take 5 $ iterate2 (^2) 2

-- intersperse
intersperse2 :: a -> [a] -> [a]
intersperse2 x []      = []
intersperse2 x (y:ys)  = y:x:intersperse2 x ys
-- intersperse2 '.' "Haskell"

-- transpose
transpose2 :: [[a]] -> [[a]]
transpose2 x = [map (!! i) x | i <- [0..length x - 1]]
-- transpose2 [[1,2,3],[1,2,3],[1,2,3]]

-- concat
concat2 :: [[a]] -> [a]
concat2 [xs]     = xs
concat2 (xs:xss) = [x | x <- xs] ++ concat2 xss
-- concat2 [[1,2,3],[1,2,3],[1,2,3]]

-- and
and2 :: [Bool] -> Bool
and2 []     = True
and2 (x:xs) = if x then and2 xs else False
-- and2 [True, False, True]

-- strict folds rfold', lfold' etc break laziness to spare memory

-- takeWhile
takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 _ []     = []
takeWhile2 f (x:xs) = if f x then (x:takeWhile2 f xs) else []
-- takeWhile2 (>3) [6,5,4,2,1]

-- span
span2 :: (a -> Bool) -> [a] -> ([a], [a]) -- Haskell still can infer this type itself
span2 f x = (start, drop (length start) x)
            where start = takeWhile2 f x
-- span2 (>3) [6,5,4,2,1]

-- drop
drop2 n xs
    | n <= 0   = xs
drop2 _ []     = []
drop2 n (x:xs) = drop2 (n-1) xs
-- drop2 3 [6,5,4,2,1]

-- group
group2 :: Eq t => [t] -> [[t]]
group2 []     = []
group2 (x:xs) = [fst spanned] ++ group2 (snd spanned)
                where spanned = span2 (==x) (x:xs)
-- group2 [1,1,1,2,2,3,4]

-- partition
partition2 :: (a -> Bool) -> [a] -> ([a], [a])
partition2 f x = (filter f x, filter (not . f) x)
-- partition2 (>3) [2,3,4,5,4,3,2,1,7]

-- findIndices
findIndices2 f x = map fst $ filter (f . snd) (zip [1..] x)
-- findIndices2 odd [x^2 | x <- [1..10]]

-- [1..10] \\ [2,5,9] list difference, union (dels dupl), intersect, nub (!) (dels dupl)







