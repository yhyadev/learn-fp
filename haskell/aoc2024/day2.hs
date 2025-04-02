increasingChecker [] = True
increasingChecker [x] = True
increasingChecker (x : xs)
  | abs diff < 1 = False
  | abs diff > 3 = False
  | diff < 0 = False
  | diff > 0 = increasingChecker xs
  where
    diff = head xs - x

decreasingChecker [] = True
decreasingChecker [x] = True
decreasingChecker (x : xs)
  | abs diff < 1 = False
  | abs diff > 3 = False
  | diff > 0 = False
  | diff < 0 = decreasingChecker xs
  where
    diff = head xs - x

isSafeReport :: [Int] -> Bool
isSafeReport (x : xs)
  | abs diff < 1 = False
  | abs diff > 3 = False
  | diff > 0 = increasingChecker xs
  | diff < 0 = decreasingChecker xs
  where
    diff = head xs - x

solve :: String -> Int
solve input = safe_reports_count
  where
    all_reports = map (map read . words) (lines input) :: [[Int]]
    safe_reports_count = length [() | levels <- all_reports, isSafeReport levels]

main :: IO ()
main = interact $ show . solve
