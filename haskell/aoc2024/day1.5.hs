import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (sort)

counter :: [Int] -> IntMap Int -> IntMap Int
counter [] cm = cm
counter (x:xs) cm = case IM.lookup x cm of
    Just count -> counter xs (IM.insert x (count + 1) cm)
    Nothing -> counter xs (IM.insert x 1 cm)

calculateScore :: [Int] -> IntMap Int -> Int -> Int
calculateScore [] cm score = score
calculateScore (x:xs) cm score = case IM.lookup x cm of
    Just count -> calculateScore xs cm (x * count + score)
    Nothing -> calculateScore xs cm score

solve :: String -> Int
solve input = calculateScore left_list right_list_counter_map 0
    where
        all_lists = map read (words input) :: [Int]
        left_list = sort [x | (x, i) <- zip all_lists [0..], odd i]
        right_list = sort [x | (x, i) <- zip all_lists [0..], even i]
        right_list_counter_map = counter right_list IM.empty

main :: IO ()
main = interact $ show . solve
