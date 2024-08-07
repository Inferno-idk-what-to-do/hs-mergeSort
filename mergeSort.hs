import Data.Function (on)
import Control.Monad (liftM2)

mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] [] = []
mergeLists x []  = x
mergeLists [] y  = y
mergeLists (x:xs) (y:ys)
  | x < y     = x : mergeLists xs  (y:ys)
  | otherwise = y : mergeLists (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = liftM2 (mergeLists `on` mergeSort) (take h)  (drop h) xs
                  where h = flip div 2 $ length xs

main :: IO ()
main = print $ mergeSort . reverse $ [1..20]
