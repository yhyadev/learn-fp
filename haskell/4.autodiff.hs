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

data Symbolic
  = Symbol String
  | Number Double
  | Function Symbolic Symbolic
  | Binary Symbolic String Symbolic
  deriving (Eq)

instance Show Symbolic where
  show (Symbol a) = a
  show (Number a) = show a
  show (Function f a) = show f ++ "(" ++ show a ++ ")"
  show (Binary a o b) = "(" ++ show a ++ " " ++ o ++ " " ++ show b ++ ")"

addition a = Binary a "+"
subtraction a = Binary a "-"
division a = Binary a "/"
multiplication a = Binary a "*"
exponentiation a = Binary a "**"

instance Num Symbolic where
  fromInteger = Number . fromInteger

  (Number a) + (Number b) = Number (a + b)
  (Number 0) + a = a
  a + (Number 0) = a
  a + b = if a == b then multiplication a 2 else addition a b

  (Number a) - (Number b) = Number (a - b)
  a - (Number 0) = a
  a - b = subtraction a b

  (Number 0) * _ = Number 0
  _ * (Number 0) = Number 0
  a * (Number 1) = a
  (Number a) * (Number b) = Number (a * b)
  a * b = multiplication a b

  abs = Function (Symbol "abs")
  negate = Function (Symbol "negate")
  signum = Function (Symbol "signum")

instance Fractional Symbolic where
  fromRational = Number . fromRational

  (Number a) / (Number b) = Number (a / b)
  a / b = division a b

  recip = (1 /)

instance Floating Symbolic where
  (Number 0) ** _ = Number 0
  _ ** (Number 0) = Number 1
  a ** (Number 1) = a
  (Number a) ** (Number b) = Number (a ** b)
  a ** b = exponentiation a b

  pi = Symbol "pi"
  exp = exponentiation (Symbol "e")
  log = Function (Symbol "log")
  logBase b = Function (Symbol $ "log" ++ show b)
  sqrt = Function (Symbol "sqrt")
  sin = Function (Symbol "sin")
  cos = Function (Symbol "cos")
  tan = Function (Symbol "tan")
  asin = Function (Symbol "asin")
  acos = Function (Symbol "acos")
  atan = Function (Symbol "atan")
  sinh = Function (Symbol "sinh")
  cosh = Function (Symbol "cosh")
  tanh = Function (Symbol "tanh")
  asinh = Function (Symbol "asinh")
  acosh = Function (Symbol "acosh")
  atanh = Function (Symbol "atanh")

newtonMethod f x = x - fx / f'x
  where
    fr = f (D x 1)
    fx = real fr
    f'x = dual fr

newtonMethodIteration _ x 0 = x
newtonMethodIteration f x i = newtonMethodIteration f (newtonMethod f x) (i - 1)

newtonRoot a b = newtonMethodIteration f b 10
  where
    f c = c ** D a 0 - D b 0
