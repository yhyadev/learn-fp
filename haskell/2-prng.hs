import Data.Bits
import Data.Time
import Data.Time.Clock.System
import Data.Word

type SplitMixState = Word64

splitMix :: SplitMixState -> (Word64, SplitMixState)
splitMix s = (d, a)
  where
    a = s + 0x9e3779b97f4a7c15
    b = xor a (shiftR a 30) * 0xbf58476d1ce4e5b9
    c = xor b (shiftR b 27) * 0x94d049bb133111eb
    d = xor c (shiftR c 31)

type Xoshiro256State = (Word64, Word64, Word64, Word64)

xoshiroNew :: Word64 -> Xoshiro256State
xoshiroNew seed = (a, b, c, d)
  where
    (as, a) = splitMix seed
    (bs, b) = splitMix as
    (cs, c) = splitMix bs
    (_, d) = splitMix cs

xoshiroNext :: Xoshiro256State -> (Word64, Xoshiro256State)
xoshiroNext (a, b, c, d) = (v, (na, nb, nc, nd))
  where
    v = a + rotateL (a + d) 23

    pc = xor c a
    pd = xor d b
    nb = xor b pc
    na = xor a pd

    nc = xor pc (shiftL b 17)

    nd = rotateL pd 45

random8 :: Xoshiro256State -> (Word64, Xoshiro256State)
random8 s = (v `mod` (1 `shiftL` 8 - 1), ns)
  where
    (v, ns) = xoshiroNext s

random16 :: Xoshiro256State -> (Word64, Xoshiro256State)
random16 s = (v `mod` (1 `shiftL` 16 - 1), ns)
  where
    (v, ns) = xoshiroNext s

random32 :: Xoshiro256State -> (Word64, Xoshiro256State)
random32 s = (v `mod` (1 `shiftL` 32 - 1), ns)
  where
    (v, ns) = xoshiroNext s

random64 :: Xoshiro256State -> (Word64, Xoshiro256State)
random64 = xoshiroNext

main :: IO ()
main = do
  utcTime <- getCurrentTime

  let systemTime = utcToSystemTime utcTime

  let state = xoshiroNew $ fromInteger $ toInteger $ systemNanoseconds systemTime

  let a : b : c : d : _ =
        map
          (\f -> fst (f state))
          [random8, random16, random32, random64]

  putStrLn (show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d)
