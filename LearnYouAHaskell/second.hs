-- ghci
-- :l C:\Haskell\LearnYouAHaskell\second.hs
-- :r reload after source change

-- import x is Python's from x import *
-- for import x as y use import qualified x as y
import Data.List
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

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


-- DATA.CHAR
-- isControl, isUpper, isSpace, isDigit...
-- all isAlphaNum "string"
-- generalCategory 'A' gives UppercaseLetter etc.
-- toUpper, toLower


-- DATA.MAP - dictionaries
dic =  Map.fromList [("key1", 5), ("key2", 4)]
-- need same types again: dic :: Map.Map [Char] Integer
dic2 = Map.insert "a" 7 dic
-- Map.size dic2
-- Map.lookup "key1" dic2
-- Map.member "key1" dic = Python key1 in dic
-- Map.map, Map.filter, Map.keys, Map.elems
-- processing duplicates: Map.fromListWith max [(2,3),(2,5),(2,100),(3,29)] 


-- DATA.SET
set = Set.fromList [1,1,2,3]


-- create module
-- module Filename  
-- ( exportFunc1  
-- , exportFunc2  
-- ) where 
-- for submodules, just create folders

-- CREATING NEW TYPES
-- data Bool = True | False
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
data Point = Point Float Float deriving (Show)
-- :t Circle   Circle :: Point -> Float -> Shape
-- :t Rectangle (Point 1 2) (Point 2 4)   :t Rectangle (Point 1 2) (Point 2 4) :: Shape

circum :: Shape -> Float
circum (Circle _ r)                            = 2 * pi * r
circum (Rectangle (Point x1 y1) (Point x2 y2)) = 2*(abs $ x1 - x2) + 2*(abs $ y1 - y2)
-- circum $ Circle (Point 0 0) 1
-- in module, export type with all constructors: Shape(..)

data Record = RecordC { name  :: String
                      , shape :: Shape
                      , num   :: Float
                      } deriving (Show)
-- RecordC {name="Shape1", shape=Circle (Point 1 2) 3, num=7}

-- type parameters
-- data Maybe a = Nothing | Just a    can produce many types
-- note: Nothing :: Maybe a, 10 :: Num t => t, [] :: [t]  type of these vals is polymorphic
-- data <type constructor> = <value constructor>

-- deriving: Haskell can automatically make cust type an instance of: Eq, Ord, Enum, Bounded, Show, Read
-- e.g. deriving enum allows us use succ, pred, take ranges (..)

-- type is for just making synonyms
-- type String = [Char] 
-- type AssocList k v = [(k,v)]  

-- recursive types, e.g. data List a = Empty | Cons a (List a) deriving...
--                       data List a = Empty | a : (List a) deriving...
data Tree1 a = EmptyTree1 | Node1 a (Tree1 a) (Tree1 a) deriving (Show, Read, Eq)
-- Node1 "a" EmptyTree EmptyTree :: Tree1 [Char]
-- Node1 3 (Node1 4 EmptyTree1 EmptyTree1) (Node1 5 EmptyTree1 EmptyTree1) :: Num a => Tree1 a

-- try to make it more succint, hijack unused ":-". Todo: overload ":"? :t (:)
-- also make it not only binary
data Tree a = Leaf a | a :- [Tree a] deriving (Show, Read, Eq)
-- 5 :- [Leaf 6, Leaf 7] :: Num a => Tree a
-- 1 :- [2 :- [5 :- [Leaf 6, Leaf 7], Leaf 7], Leaf 8]

-- :i Num for typeclass info

-- instance with constraints
-- instance (Eq m) => Eq (Maybe m) where  
--     Just x == Just y = x == y  
--     Nothing == Nothing = True  
--     _ == _ = False 

-- typeclass FUNCTOR
-- :i Functor

-- KINDS
-- kind is "type of a type"
-- :k Int gives Int :: *   - concrete type
-- Maybe :: * -> *   - takes concrete type, returns concrete type
-- Maybe Int :: Int -> *
