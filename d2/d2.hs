import Data.Bits (xor)
import Data.List (elemIndex)
import Data.Maybe (fromJust, isNothing)

readf = do
  content <- readFile "d2inp.txt"
  return (split '\n' content)

main = do
  content <- readf
  print (p2 content)

split _ [] =
  []
split d s =
  case xs of
    [] -> x : split d xs
    _ -> x : split d (tail xs)
  where
    (x, xs) = break (== d) s

p1 :: [String] -> Int
p1 [] =
  0
p1 (s : ss) =
  fromEnum (occurances <= read maxi && occurances >= read mini) + p1 ss
  where
    (r, _ : pw) = break (== ':') s
    (num, _ : c) = break (== ' ') r
    (mini, _ : maxi) = break (== '-') num
    occurances = (length . filter (== head c)) pw

p2 :: [String] -> Int
p2 [] =
  0
p2 (s : ss) =
  fromEnum (xor (head c == pw !! read pos1) (head c == pw !! read pos2)) + p2 ss
  where
    (r, _ : pw) = break (== ':') s
    (num, _ : c) = break (== ' ') r
    (pos1, _ : pos2) = break (== '-') num