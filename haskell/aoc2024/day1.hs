import Data.List (sort)

solve :: String -> Integer
solve input = sum distances
    where
        all_lists = map read (words input) :: [Integer]
        left_list = sort [x | (x, i) <- zip all_lists [0..], odd i]
        right_list = sort [x | (x, i) <- zip all_lists [0..], even i]
        distances = [abs (x - y) | (x, y) <- zip left_list right_list]

main :: IO ()
main = interact $ show . solve
