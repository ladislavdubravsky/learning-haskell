import System.IO
import Control.Monad
import qualified Data.Map.Strict as M

roomtype 1 = M.fromList [("TOP", "D"),("LEFT","D"),("RIGHT","D")]
roomtype 2 = M.fromList [("LEFT","R"),("RIGHT","L")]
roomtype 3 = M.fromList [("TOP","D")]
roomtype 4 = M.fromList [("TOP","L"),("RIGHT","D")]
roomtype 5 = M.fromList [("TOP","R"),("LEFT","D")]
roomtype 6 = M.fromList [("TOP","RL"),("RIGHT","L"),("LEFT","R")]
roomtype 7 = M.fromList [("TOP","D"),("RIGHT","D")]
roomtype 8 = M.fromList [("RIGHT","D"),("LEFT","D")]
roomtype 9 = M.fromList [("TOP","D"),("LEFT","D")]
roomtype 10 = M.fromList [("TOP","L")]
roomtype 11 = M.fromList [("TOP","R")]
roomtype 12 = M.fromList [("RIGHT","D")]
roomtype 13 = M.fromList [("LEFT","D")]

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    [w,h] <- map (read :: String -> Int) . words <$> getLine
    temple <- replicateM h (map (read :: String -> Int) . words <$> getLine)
    getLine
    loop temple

loop temple = do
    input <- words <$> getLine
    let (xi,yi,pos) = (read (input!!0) :: Int, read (input!!1) :: Int, input!!2)
    let dir2coord = M.fromList [("D",(xi,yi+1)), ("L",(xi-1,yi)), ("R",(xi+1,yi))]
    let res = (dir2coord)M.!((roomtype $ temple!!yi!!xi)M.!pos)
    putStrLn ((show $ fst res) ++ " " ++ (show $ snd res))  
    loop temple
