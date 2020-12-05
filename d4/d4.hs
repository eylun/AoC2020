import Data.Char (isAlphaNum, isNumber)
import Data.Maybe (fromJust, isNothing)
import Text.Read

readf = do
  content <- readFile "d4inp.txt"
  return (split "" (split '\n' content))

split :: Eq a => a -> [a] -> [[a]]
split _ [] =
  []
split d s =
  takeWhile (/= d) s : split d (if null nxt then nxt else tail nxt)
  where
    nxt = dropWhile (/= d) s

type Passport = [(String, String)]

clean :: [[String]] -> [Passport]
clean [] =
  []
clean (p : ps) =
  fields : clean ps
  where
    fields = map (\x -> let (a : b : _) = split ':' x in (a, b)) (concatMap words p)

main = do
  content <- readf
  print (p2 (clean content))

p1 :: [Passport] -> Int
p1 =
  foldr ((+) . fromEnum . (not . validCheck)) 0

validCheck :: Passport -> Bool
validCheck p =
  any (isNothing . flip lookup p) check
  where
    check = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

p2 :: [Passport] -> Int
p2 =
  foldr ((+) . fromEnum . p2Check) 0

p2Check :: Passport -> Bool
p2Check p =
  not (validCheck p) && all (`conditionCheck` p) check
  where
    check = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

-- conditionCheck :: String -> Passport -> Bool
conditionCheck "byr" p =
  byr >= 1920 && byr <= 2002
  where
    byr = (read . fromJust . lookup "byr") p
conditionCheck "iyr" p =
  iyr >= 2010 && iyr <= 2020
  where
    iyr = (read . fromJust . lookup "iyr") p
conditionCheck "eyr" p =
  eyr >= 2020 && eyr <= 2030
  where
    eyr = (read . fromJust . lookup "eyr") p
conditionCheck "hgt" p
  | ntype == "cm" = num' >= 150 && num' <= 193
  | ntype == "in" = num' >= 59 && num' <= 76
  | otherwise = False
  where
    num' = read num
    (num, ntype) = heightChecker hgt
    hgt = (fromJust . lookup "hgt") p
    heightChecker [] =
      ("", "")
    heightChecker "cm" =
      ("", "cm")
    heightChecker "in" =
      ("", "in")
    heightChecker (c : cs) =
      (c : num, ntype)
      where
        (num, ntype) = heightChecker cs
conditionCheck "hcl" p
  | hcl /= '#' = False
  | otherwise = all isAlphaNum hcl' && length hcl' == 6
  where
    (hcl : hcl') = (fromJust . lookup "hcl") p
conditionCheck "ecl" p =
  ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  where
    ecl = (fromJust . lookup "ecl") p
conditionCheck "pid" p =
  length pid == 9 && all isNumber pid
  where
    pid = (fromJust . lookup "pid") p