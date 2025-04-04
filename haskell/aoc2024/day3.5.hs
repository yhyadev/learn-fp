import Data.Char
import Data.Text qualified as T

eval :: Bool -> String -> Integer
eval _ [] = 0
eval _ ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : xs) = eval False xs
eval _ ('d' : 'o' : '(' : ')' : xs) = eval True xs
eval enabled ('m' : 'u' : 'l' : '(' : xs)
  | head xs' /= ',' || head xs'' /= ')' = eval enabled xs
  | not enabled = eval enabled (tail xs'')
  | otherwise = first_integer * second_integer + eval enabled (tail xs'')
  where
    first_integer_digits = takeWhile isDigit xs
    first_integer = read first_integer_digits :: Integer

    xs' = drop (length first_integer_digits) xs

    second_integer_digits = takeWhile isDigit $ tail xs'
    second_integer = read second_integer_digits :: Integer

    xs'' = drop (length second_integer_digits) $ tail xs'
eval enabled xs = eval enabled $ tail xs

main = interact $ show . eval True
