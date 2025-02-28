fib :: Integer -> Integer
fib = internal 0 1
    where
        internal :: Integer -> Integer -> Integer -> Integer
        internal a b 0 = a
        internal a b c = internal b (a + b) (c - 1)

printEach :: Show a => [a] -> IO ()
printEach [a] = print a
printEach (a:rest) = do
    print a
    printEach rest

main :: IO ()
main = do
  putStrLn "Fibonacci Sequence upto 25: "
  printEach $ map fib [0..25]
