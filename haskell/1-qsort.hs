qsort :: (a -> a -> Bool) -> [a] -> [a]
qsort f [] = []
qsort f [x] = [x]
qsort f (pivot : rest) =
  qsort f (filter (`f` pivot) rest) ++ [pivot] ++ qsort f (filter (not . (`f` pivot)) rest)
