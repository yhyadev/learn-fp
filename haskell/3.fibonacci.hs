fib :: Integer -> Integer
fib = helper 0 1
    where
        helper :: Integer -> Integer -> Integer -> Integer
        helper a b 0 = a
        helper a b c = helper b (a + b) (c - 1)

fibseq :: Integer -> IO ()

fibseq 50 = do
    print $ fib 50

fibseq n = do
    print $ fib n
    fibseq (n + 1)

main :: IO ()
main = do
  putStrLn "Fibonacci Sequence upto 20: "
  fibseq 0
