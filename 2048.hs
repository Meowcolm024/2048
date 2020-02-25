-- 2048 in Haskell
import           Data.List.Split                ( chunksOf )
import           Data.List                      ( transpose, group )
import           System.Random                  ( randomRIO )
import           Control.Monad                  ( replicateM )

data Grid = Grid [[Int]] deriving (Show, Eq)
data Act = Ls | Rs | Us | Ds deriving (Show, Eq)

main :: IO ()
main = do
  st <- gen $ Grid $ chunksOf 4 $ replicate 16 0
  showScore st
  showGrid st
  loop st

loop :: Grid -> IO ()
loop x = do
  putStr "\nType in the control (WASD): "
  cmd <- getLine
  out <- gen $ move (act $ head cmd) x
  showScore out
  showGrid out
  loop out

showGrid :: Grid -> IO ()
showGrid (Grid x) = mapM_ (putStrLn . f) x where
  f m = unwords $ map (\y -> if y == 0 then "_" else show y) m

showScore :: Grid -> IO ()
showScore (Grid x) = do
  let t = (sum . map sum) x
  let m = (maximum . map maximum) x
  putStrLn "--------------------------"
  putStrLn $ "Total: " ++ show t ++ " Max: " ++ show m
  putStrLn "--------------------------"

step :: [Int] -> [Int]
step x = y ++ replicate (4 - length y) 0
 where
  y = (concatMap merge . concatMap (chunksOf 2) . group . filter (/= 0)) x
  merge n = case n of
    [a, b] -> if a == b then [a + b] else [a, b]
    _      -> n

move :: Act -> Grid -> Grid
move d (Grid x) = case d of
  Ls -> Grid $ map step x
  Rs -> Grid $ map (reverse . step . reverse) x
  Us -> Grid $ transpose . map step . transpose $ x
  Ds -> Grid $ transpose . map (reverse . step . reverse) . transpose $ x

gen :: Grid -> IO Grid
gen (Grid g) = do
  rnd <-
    replicateM 16
    $   (\x y z -> x * y * z)
    <$> randomRIO (0, 1)
    <*> randomRIO (0, 1)
    <*> randomRIO (1, 2)
  return $ Grid $ chunksOf 4 $ add [] (concat g) (map (* 2) rnd)

add :: [Int] -> [Int] -> [Int] -> [Int]
add org (x : xs) (y : ys) =
  if x == 0 then add (org ++ [y]) xs ys else add (org ++ [x]) xs ys
add org _ _ = org

act :: Char -> Act
act x = case x of
  'a' -> Ls
  'd' -> Rs
  'w' -> Us
  's' -> Ds
