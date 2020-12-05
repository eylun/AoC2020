readf = do
  content <- readFile "d3inp.txt"
  return (split '\n' content)

split :: Eq a => a -> [a] -> [[a]]
split _ [] =
  []
split d s =
  takeWhile (/= d) s : split d (if null nxt then nxt else tail nxt)
  where
    nxt = dropWhile (/= d) s

main = do
  content <- readf
  print (p2 content [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)])

type Position = (Int, Int)

type YPos = Int

type Movement = (Int, Int)

p1 :: [String] -> Position -> Int
p1 [_] _ =
  0
p1 tmap@(r : rs) (x, y) =
  fromEnum ((tmap !! 1 !! newY) == '#') + p1 rs (x + 1, newY)
  where
    newY = mod (y + 3) (length r)

p2 :: [String] -> [Movement] -> Int
p2 _ [] =
  1
p2 tmap (m : ms) =
  p2' tmap m 0 * p2 tmap ms
  where
    p2' :: [String] -> Movement -> Int -> Int
    p2' [] _ _ =
      0
    p2' tmap' m@(mx, my) posY =
      fromEnum (r !! posY == '#') + p2' rs m newY
      where
        newY = mod (posY + my) (length r)
        r = head tmap'
        rs = drop mx tmap'