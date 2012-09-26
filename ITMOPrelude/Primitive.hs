{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read,error)

---------------------------------------------
-- Синтаксис лямбда-выражений
-- I HATE NON-ASCII SYMBOLS IN THE CODE!

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- Зацикленное выражение
undefined = undefined

-- Ниже следует реализовать все термы, состоящие из undefined заглушки.
-- Любые термы можно переписывать (natEq и natLt --- хорошие кандидаты).

-------------------------------------------
-- Примитивные типы

-- Тип с единственным элементом
data Unit = Unit deriving (Show,Read)

-- Пара, произведение
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- Вариант, копроизведение
data Either a b = Left a | Right b deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- Следует отметить, что встроенный if с этим Bool использовать нельзя,
-- зато case всегда работает.

-- Ну или можно реализовать свой if
if' True a b = a
if' False a b = b

-- Трихотомия. Замечательный тип, показывающий результат сравнения
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- Булевы значения

-- Логическое "НЕ"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- Логическое "И"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- Логическое "ИЛИ"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- Натуральные числа

data Nat = Zero | Succ Nat deriving (Show,Read) -- Zero is not a natural number...

natZero = Zero     -- 0
natOne = Succ Zero -- 1

-- Сравнивает два натуральных числа
natCmp :: Nat -> Nat -> Tri
natCmp Zero     Zero     = EQ
natCmp Zero     (Succ _) = LT
natCmp (Succ _) Zero     = GT
natCmp (Succ n) (Succ m) = natCmp n m

-- n совпадает с m 
natEq :: Nat -> Nat -> Bool
natEq n m = case natCmp n m of
    EQ -> True
    _  -> False

-- n меньше m
natLt :: Nat -> Nat -> Bool
natLt n m = case natCmp n m of
    LT -> True
    _  -> False

natGt :: Nat -> Nat -> Bool
natGt n m = case natCmp n m of
    GT -> True
    _  -> False

infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
n -. Zero = n
(Succ n) -. (Succ m) = n -. m
Zero -. _ = error "!!: negative result"

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- Целое и остаток от деления n на m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod _ Zero = error "!!: divizion by zero"
natDivMod n m = if' (natLt n m) (Pair natZero n) (Pair (Succ (natDiv (n -. m) m)) (natMod (n -. m) m))

natDiv n = fst . natDivMod n -- Целое
natMod n = snd . natDivMod n -- Остаток

-- Поиск GCD алгоритмом Евклида (должен занимать 2 (вычислителельная часть) + 1 (тип) строчки)
gcd :: Nat -> Nat -> Nat
gcd n Zero = n
gcd n m = gcd m (natMod n m)

-------------------------------------------
-- Целые числа

-- Требуется, чтобы представление каждого числа было единственным
data Int = Positive Nat | Negative Nat deriving (Show,Read)

intZero   = Positive Zero -- 0
intOne    = Positive $ Succ Zero -- 1
intNegOne = Negative Zero -- -1

-- n -> - n
intNeg :: Int -> Int
intNet intZero = intZero
intNeg (Positive (Succ n)) = Negative n
intNeg (Negative n) = Positive $ Succ n

-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp (Positive _) (Negative _) = GT
intCmp (Negative _) (Positive _) = LT
intCmp (Positive n) (Positive m) = natCmp n m
intCmp (Negative n) (Negative m) = natCmp m n

intEq :: Int -> Int -> Bool
intEq n m = case intCmp n m of
    EQ -> True
    _  -> False

intLt :: Int -> Int -> Bool
intLt n m = case intCmp n m of
    LT -> True
    _  -> False

infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
(Positive n) .+. (Positive m) = Positive $ n +. m
(Negative n) .+. (Negative m) = Negative $ Succ (n +. m)
(Positive Zero) .+. (Negative m) = Negative m
(Positive (Succ n)) .+. (Negative m) = case natLt m (Succ n) of
    True -> Positive $ n -. m
    False -> Negative $ m -. (Succ n)
(Negative n) .+. (Positive m) = (Positive m) .+. (Negative n)

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
(Positive n) .*. (Positive m) = Positive $ n *. m
(Negative n) .*. (Negative m) = Positive $ (Succ n) *. (Succ m)
(Positive n) .*. (Negative m) = intNeg $ Positive $ n *. (Succ m)
(Negative n) .*. (Positive m) = (Positive m) .*. (Negative n)

-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat deriving (Show,Read)

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- У рациональных ещё есть обратные элементы
ratInv :: Rat -> Rat
ratInv (Rat (Positive x) y) = Rat (Positive y) x
ratInv (Rat (Negative x) y) = Rat (intNeg $ Positive y) (Succ x)

-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp (Rat a b) (Rat c d) = intCmp (a .*. (Positive d)) (c .*. (Positive b))

ratEq :: Rat -> Rat -> Bool
ratEq x y = case ratCmp x y of
    EQ -> True
    _  -> False

ratLt :: Rat -> Rat -> Bool
ratLt x y = case ratCmp x y of
    LT -> True
    _  -> False

-- TODO normalization
ratNorm :: Rat -> Rat
ratNorm (Rat (Positive x) y) = Rat (Positive $ natDiv x g) (natDiv y g) where
    g = gcd x y

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
(Rat a b) %+ (Rat c d) = Rat ((a .*. (Positive d)) .+. (c .*. (Positive b))) (b *. d)

(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat a b) %* (Rat c d) = Rat (a .*. c) (b *. d)

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

-------------------------------------------
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b
