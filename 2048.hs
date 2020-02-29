-- 2048 in Haskell
import           Data.List.Split                ( chunksOf )
import           Data.List                      ( transpose, group )
import           System.Random                  ( randomRIO )
import           Control.Monad                  ( replicateM )

data Grid = Grid [[Int]] deriving (Show, Eq)
data Act = Ls | Rs | Us | Ds deriving (Show, Eq)

main :: IO ()
main = gen (Grid $ chunksOf 4 $ replicate 16 0) >>= loop

loop :: Grid -> IO ()
loop x = do
  showScore x
  showGrid x
  putStr "\nType in the control (WASD): "
  cmd <- getLine
  gen (move (act $ head cmd) x) >>= loop

showGrid :: Grid -> IO ()
showGrid (Grid x) = mapM_ (putStrLn . f) x where
  f m = unwords $ map (\y -> if y == 0 then "_" else show y) m

showScore :: Grid -> IO ()
showScore (Grid x) = do
  putStrLn "--------------------------"
  putStr   $  "Total: " ++ (show . sum . concat) x 
  putStrLn $ " Max: "  ++ (show . maximum . concat) x
  putStrLn "--------------------------"

step :: [Int] -> [Int]
step x = y ++ replicate (4 - length y) 0
 where
  y = map sum . concatMap (chunksOf 2) . group . filter (/= 0) $ x

move :: Act -> Grid -> Grid
move d (Grid x) = case d of
  Ls -> Grid $ map step x
  Rs -> Grid $ map (reverse . step . reverse) x
  Us -> Grid $ transpose . map step . transpose $ x
  Ds -> Grid $ transpose . map (reverse . step . reverse) . transpose $ x

gen :: Grid -> IO Grid
gen (Grid g) = do
  r <-
    replicateM 16
    $   (\x y z -> x * y * z * 2)
    <$> randomRIO (0, 1)
    <*> randomRIO (0, 1)
    <*> randomRIO (0, 2)
  return $ Grid $ chunksOf 4 
                $ zipWith (\x y -> if x == 0 then y else x) (concat g) r

act :: Char -> Act
act x = case x of
  'a' -> Ls
  'd' -> Rs
  'w' -> Us
  's' -> Ds
