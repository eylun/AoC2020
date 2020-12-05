import Data.List

readf = do
  content <- readFile "d5inp.txt"
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

-- to get index, (last - first) // 2 + 1

p1 :: [String] -> Int
p1 [] = 0
p1 (p : ps) =
  max (halver fb (0, 127) * 8 + halver lr (0, 7)) (p1 ps)
  where
    fb = take 7 p
    lr = drop 7 p
    halver [] (l, _) =
      l
    halver (c : cs) (l, h)
      | c == 'F' || c == 'L' = halver cs (l, l + div (h - l) 2)
      | otherwise = halver cs (l + div (h - l) 2 + 1, h)

p2 :: [String] -> Int
p2 p =
  snd (head (filter (\(x, y) -> x /= y) (zip ids [head ids ..])))
  where
    ids = sort (helper p)

helper :: [String] -> [Int]
helper [] = []
helper (p : ps) =
  row * 8 + col : helper ps
  where
    row = helper' (take 7 p) (0, 127)
    col = helper' (drop 7 p) (0, 7)
    helper' [] (l, _) = l
    helper' (c : cs) (l, h)
      | c == 'F' || c == 'L' = helper' cs (l, l + div (h - l) 2)
      | otherwise = helper' cs (l + div (h - l) 2 + 1, h)