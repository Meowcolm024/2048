-- 2048 in Haskell
import           Data.List.Split                ( chunksOf )
import           Data.List                      ( transpose, group )
import           System.Random                  ( randomRIO )
import           Control.Monad                  ( replicateM )
import           System.IO                      ( hFlush, stdout )

type Grid = [[Int]]
data Act = Ls | Rs | Us | Ds deriving (Show, Eq)

main :: IO ()
main = gen (chunksOf 4 $ replicate 16 0) >>= loop

loop :: Grid -> IO ()
loop x = do
  showScore x >> showGrid x
  putStr "\nType in the control (WASD): " >> hFlush stdout
  getLine >>= \y -> let a = act . head $ y ++ " " in case a of
    Just a' -> gen (move a' x) >>= loop
    Nothing -> loop x

showGrid :: Grid -> IO ()
showGrid = mapM_ (putStrLn . f)
  where f = unwords . map (\y -> if y == 0 then "_" else show y)

showScore :: Grid -> IO ()
showScore x = do
  putStrLn "--------------------------"
  putStr   $ "Total: " ++ (show . sum . concat) x
  putStrLn $ " Max: " ++ (show . maximum . concat) x
  putStrLn "--------------------------"

step :: [Int] -> [Int]
step x = y ++ replicate (4 - length y) 0
  where y = map sum . concatMap (chunksOf 2) . group . filter (/= 0) $ x

move :: Act -> Grid -> Grid
move d = case d of
  Ls -> map step
  Rs -> map (reverse . step . reverse)
  Us -> transpose . map step . transpose
  Ds -> transpose . map (reverse . step . reverse) . transpose

gen :: Grid -> IO Grid
gen g = chunksOf 4 . zipWith (\x y -> if x == 0 then y else x) (concat g)
  <$> replicateM 16
    ((\ x y z -> x * y * z * 2) 
    <$> randomRIO (0, 1) 
    <*> randomRIO (0, 1)
    <*> randomRIO (0, 2))

act :: Char -> Maybe Act
act x = case x of
  'a' -> Just Ls
  'd' -> Just Rs
  'w' -> Just Us
  's' -> Just Ds
  _   -> Nothing
