increasingChecker [] = True
increasingChecker [x] = True
increasingChecker (x : xs)
  | diff <= 0 || diff > 3 = False
  | diff > 0 = increasingChecker xs
  where
    diff = head xs - x

decreasingChecker [] = True
decreasingChecker [x] = True
decreasingChecker (x : xs)
  | diff >= 0 || diff < -3 = False
  | diff < 0 = decreasingChecker xs
  where
    diff = head xs - x

reportChecker :: [Int] -> Bool
reportChecker (x : xs)
  | diff == 0 || abs diff > 3 = False
  | diff > 0 = increasingChecker xs
  | diff < 0 = decreasingChecker xs
  where
    diff = head xs - x

reportCheckerE :: [Int] -> [Int] -> Bool
reportCheckerE l [] = False
reportCheckerE l r
  | reportChecker (l ++ r) = True
  | reportChecker (l ++ tail r) = True
  | reportCheckerE (l ++ [head r]) (tail r) = True
  | otherwise = False

solve :: String -> Int
solve input = safe_reports_count
  where
    all_reports = map (map read . words) (lines input) :: [[Int]]
    safe_reports_count = length [() | levels <- all_reports, reportCheckerE [] levels]

main :: IO ()
main = interact $ show . solve
