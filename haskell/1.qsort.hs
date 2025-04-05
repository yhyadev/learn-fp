qsort :: (a -> a -> Bool) -> [a] -> [a]
qsort f [] = []
qsort f [x] = [x]
qsort f (pivot : rest) =
  qsort f (filter (`f` pivot) rest) ++ [pivot] ++ qsort f (filter (not . (`f` pivot)) rest)

main :: IO ()
main = do
  putStr "Unsorted Array: "
  print unsorted_arr
  putStr "Sorted Array: "
  print sorted_arr
  where
    unsorted_arr = [1, 3, 5, 2, 9, 8, 10]
    sorted_arr = qsort (<) unsorted_arr
