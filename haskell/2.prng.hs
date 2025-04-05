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

xoshiroInitState :: Word64 -> Xoshiro256State
xoshiroInitState seed = (a, b, c, d)
  where
    (as, a) = splitMix seed
    (bs, b) = splitMix as
    (cs, c) = splitMix bs
    (_, d) = splitMix cs

newtype Xoshiro256 a = Xoshiro256 {xoshiroRun :: Xoshiro256State -> (Xoshiro256State, a)}

instance Functor Xoshiro256 where
  fmap f a = Xoshiro256 b
    where
      b s = (ns, f v)
        where
          (ns, v) = xoshiroRun a s

instance Applicative Xoshiro256 where
  pure x = Xoshiro256 (,x)

  Xoshiro256 a <*> Xoshiro256 b = Xoshiro256 c
    where
      c s = (ls, f v)
        where
          (ns, f) = a s
          (ls, v) = b ns

random64 :: Xoshiro256 Word64
random64 = Xoshiro256 f
  where
    f (a, b, c, d) = ((na, nb, nc, nd), v)
      where
        v = a + rotateL (a + d) 23

        pc = xor c a
        pd = xor d b
        nb = xor b pc
        na = xor a pd

        nc = xor pc (shiftL b 17)

        nd = rotateL pd 45

random32 = fmap (\x -> mod x (1 `shiftL` 32 - 1)) random64
random16 = fmap (\x -> mod x (1 `shiftL` 16 - 1)) random64
random8 = fmap (\x -> mod x (1 `shiftL` 8 - 1)) random64

main :: IO ()
main = do
  utcTime <- getCurrentTime

  let systemTime = utcToSystemTime utcTime

  let state = xoshiroInitState $ fromInteger $ toInteger $ systemNanoseconds systemTime

  let (last_state, a : b : c : d : _) =
        xoshiroRun
          ( sequenceA
              [ random64,
                random32,
                random16,
                random8
              ]
          )
          state

  putStrLn (show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d)
