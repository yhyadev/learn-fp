import Data.Char
import Data.Text qualified as T

eval :: String -> Integer
eval [] = 0
eval ('m' : 'u' : 'l' : '(' : xs)
  | head xs' /= ',' || head xs'' /= ')' = eval xs
  | otherwise = first_integer * second_integer + eval (tail xs'')
  where
    first_integer_digits = takeWhile isDigit xs
    first_integer = read first_integer_digits :: Integer

    xs' = drop (length first_integer_digits) xs

    second_integer_digits = takeWhile isDigit $ tail xs'
    second_integer = read second_integer_digits :: Integer

    xs'' = drop (length second_integer_digits) $ tail xs'
eval xs = eval $ tail xs

main = interact $ show . eval
