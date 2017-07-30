-- run without compiling:
--     runghc C:\Haskell\LearnYouAHaskell\first.hs
-- interactive interpreter:
--     ghci
--     :l C:\Haskell\LearnYouAHaskell\first.hs
--     :r reload after source change
-- compile:
--     ghc C:\Haskell\LearnYouAHaskell\first.hs
-- TODO: look into Cabal

main = putStrLn "Running first.hs. Load in ghci to test functions interactively."

-- definition
a = 7
-- a = 5 error, no redefinitions

-- simple funcs
double x = 2 * x
cond x   = if x > 10^3
           then "big"
           else "small"

-- infinite list, evaluation is lazy
inf = [1..]

-- list contents need to be same type
fin = [2,4..20]
-- :t fin    fin :: [Integer]

-- 0:fin -- prepend
-- [1,2,3] is just syntactic sugar for 1:2:3:[]
-- [1, 2] ++ [3, 4] concatenation
-- fin !! 0 element access
-- last fin -- last elem, second to last already has to use pattern matching?

str = "string" -- String is list of chars [Char]
-- head tail, last init
-- null take reverse
-- maximum, minimum, sum, product
-- 4 `elem` [3, 4] membership test, elem 4 [3, 4] to avoid the infix symbol

-- take 10 (cycle [1, 2, 3])

-- listcomps, Python's [x**2 for x in range(6)]
lc = [x^2 | x <- [0..5]]
-- [x^2 | x <- [0..5], odd x]
-- [x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]

-- zip [1,2] [3,4]
-- [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

-- declare signatures for functions
removeVowels :: [Char] -> [Char]
removeVowels str = [c | c <- str, not (elem c ['a', 'e', 'i', 'y', 'o', 'u'])]

-- types: Int, Integer (arbitratily large), Double, Bool, Char, String
giveMeAnyType :: a -> a
giveMeAnyType x = x
-- note: there is id function for this

-- typeclasses
-- :t (>)    (>) :: Ord a => a -> a -> Bool    Ord is typeclass for types that can be compared
-- :t (==)   (>) ::  Eq a => a -> a -> Bool    Eq for types testable for equality
-- Show      showable types
-- Read
-- Num       numeric types
-- Enum
-- Integral
-- Floating

-- define function on Integral types
sayMe :: (Integral a) => a -> String  -- without typespec error, we need at least (Eq a, Num a)
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"   
sayMe x = "Stuff"

-- define overloaded count for various containers: custom typeclass for mycountable types
class MyCountable a where
    myCount :: a -> Int

-- register myCounts for some types:
instance MyCountable Int where
    myCount x = 5 -- for the moment...

instance MyCountable Bool where
    myCount x = 7 -- for the moment...

-- func taking list of MyCountable types and returning myCount of first elem
funcOnMyCountable :: (MyCountable a) => [a] -> Int
funcOnMyCountable [] = 0
funcOnMyCountable (x:_) = myCount x
-- use:
-- funcOnMyCountable [4]         error because 4 is of ambiguous type
-- funcOnMyCountable [4::Int]    ok

-- custom head implementation    
myHead :: [a] -> a
myHead [] = error "Empty lists have no heads!"
myHead (x:xs) = x

-- instead of Python's __repr__: Show is typeclass of types that have show
someRepr :: (Show a) => [a] -> String
someRepr [] = "Empty list."
someRepr (x:[]) = "1-element: " ++ show x
someRepr (x:xs) = "Many elements, first: " ++ show x
-- someRepr [[1,2],[3,4]]

-- sum of list - no loops, use recursion
mySum :: (Num a) => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

-- xs@(x:y:ys) name catched pattern xs

-- guards
-- let ghc deduce funky type for following func
descrNum n
    | n < 0     = "Negative"
    | n == 0    = "Zero"
    | otherwise = "Maybe positive?"
-- :t descrNum gets us descrNum :: (Ord a, Num a) => a -> [Char]

-- where
descrShiftedNum :: (Ord a, Num a) => a -> [Char]
descrShiftedNum n
    | s < 0     = "Negative"
    | s == 0    = "Zero"
    | otherwise = "Positive or..."
    where s = n - 7

-- let <bindings> in <expression : very local bindings

-- tuples: based on length and item types each is its own type:
-- (2, "a") :: Num t => (t, [Char])

-- great algebraic style
take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs

-- currying
mult3 :: (Num a) => a -> a -> a -> a -- is a -> (a -> (a -> a))
mult3 x y z = x * y * z
-- :t mult3 5    Num a => a -> (a -> a)

-- partial application in infix notation
divideByTen = (/10) 

applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)
-- applyTwice (+3) 10

-- map, filter
-- filter (>3) [1,5,3,2,1,6,4,3,2,1]

-- lambdas: (\x -> x + 3), but partial application much better: (+3)

-- foldl, foldr, foldl1, foldr1
-- scanl, scanr, ...
-- scanl (+) 0 [3,5,2,1]
-- [0,3,8,1,11]

-- ($): right-associative low precedence function application:
-- f $ x is f x
-- f g h x is ((f g) h) x
-- f $ g $ h x is f (g (h x))
-- even better use:
-- map ($ 3) [(4+), (10*), (^2), sqrt]

-- function composition, normally "."
dot :: (b -> c) -> (a -> b) -> a -> c  
dot f g = \x -> f (g x)
-- f (g (z x)) is equivalent to (f . g . z) x - even better than $s

-- pointfree definitions:
-- sum2 xs = foldl (+) 0 xs can be just:
sum2 :: (Num a) => [a] -> a
sum2 = foldl (+) 0
