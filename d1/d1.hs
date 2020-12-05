import Data.List (elemIndex)
import Data.Maybe (fromJust, isNothing)

readf = do
  contents <- readFile "d1inp.txt"
  return . map readInt . words $ contents

readInt :: String -> Int
readInt = read

main = do
  contents <- readf
  print (p2 contents)

p1 ls =
  ls !! i1 * ls !! i2
  where
    len = length ls
    index = fromJust (elemIndex 2020 (concatMap (\x -> map (x +) ls) ls))
    (i1, i2) = quotRem index len

p2 ls =
  ls !! i1 * ls !! i2 * ls !! i3
  where
    len = length ls
    index = fromJust (elemIndex 2020 (concatMap (\y -> map (y +) (concatMap (\x -> map (x +) ls) ls)) ls))
    (i1, i23) = quotRem index (len ^ 2)
    (i2, i3) = quotRem i23 len