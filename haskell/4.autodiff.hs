data Dual a = D a a
  deriving (Functor, Eq, Ord)

instance (Show a) => Show (Dual a) where
  show (D u u') = show u ++ " + " ++ show u' ++ "e"

instance (Num a) => Num (Dual a) where
  D u u' + D v v' = D (u + v) (u' + v')
  D u u' - D v v' = D (u - v) (u' - v')
  D u u' * D v v' = D (u * v) (u * v' + v * u')

  fromInteger :: Integer -> Dual a
  fromInteger r = D (fromInteger r) 0

  negate :: Dual a -> Dual a
  negate = fmap negate

  abs :: Dual a -> Dual a
  abs (D u u') = D (abs u) (u' * signum u)

  signum :: Dual a -> Dual a
  signum = undefined

instance (Fractional a) => Fractional (Dual a) where
  D u u' / D v v' = D (u / v) ((u' * v - v' * u) / (v * v))

  recip :: Dual a -> Dual a
  recip = (1 /)

  fromRational :: Rational -> Dual a
  fromRational r = D (fromRational r) 0

instance (Floating a) => Floating (Dual a) where
  pi :: Dual a
  pi = D pi (2 * pi)

  exp :: Dual a -> Dual a
  exp (D u u') = D (exp u) (u' * exp u)

  log :: Dual a -> Dual a
  log (D u u') = D (log u) (u' / u)

  sqrt :: Dual a -> Dual a
  sqrt x = x ** (1 / 2)

  (**) :: Dual a -> Dual a -> Dual a
  (D u u') ** (D k _) = D (u ** k) (u' * k * u ** (k - 1))

  logBase :: Dual a -> Dual a -> Dual a
  logBase b x = log x / log b

  sin :: Dual a -> Dual a
  sin (D u u') = D (sin u) (u' * cos u)

  cos :: Dual a -> Dual a
  cos (D u u') = D (cos u) (-(u' * sin u))

  tan :: Dual a -> Dual a
  tan (D u u') = undefined

  sinh :: Dual a -> Dual a
  sinh (D u u') = D (sinh u) (u' * cosh u)

  cosh :: Dual a -> Dual a
  cosh (D u u') = D (cosh u) (u' * sinh u)

  tanh :: Dual a -> Dual a
  tanh (D u u') = undefined

  asinh :: Dual a -> Dual a
  asinh (D u u') = undefined

  acosh :: Dual a -> Dual a
  acosh (D u u') = undefined

  atanh :: Dual a -> Dual a
  atanh (D u u') = undefined

  asin :: Dual a -> Dual a
  asin (D u u') = undefined

  acos :: Dual a -> Dual a
  acos (D u u') = undefined

  atan :: Dual a -> Dual a
  atan (D u u') = undefined

real (D u _) = u

dual (D _ u') = u'

deriv :: (Num a) => (Dual a -> Dual a) -> a -> a
deriv f x = dual . f $ D x 1

derivX :: (Num a) => (Dual a -> Dual a -> Dual a) -> a -> a -> a
derivX f x y = dual $ f (D x 1) (D y 0)

derivY :: (Num a) => (Dual a -> Dual a -> Dual a) -> a -> a -> a
derivY f x y = dual $ f (D x 0) (D y 1)
