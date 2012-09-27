{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive

---------------------------------------------
-- Что надо делать?
--
-- Все undefined превратить в требуемые термы.
-- Звёздочкой (*) отмечены места, в которых может потребоваться думать.

---------------------------------------------
-- Определение

data List a = Nil |  Cons a (List a) deriving (Show,Read)

---------------------------------------------
-- Операции

-- Длина списка
length :: List a -> Nat
length Nil = Zero
length (Cons _ l) = Succ $ length l

-- Склеить два списка за O(length a)
(++) :: List a -> List a -> List a
Nil ++ l = l
(Cons a x) ++ y = Cons a $ x ++ y

-- Список без первого элемента
tail :: List a -> List a
tail Nil = error "ITMOPrelude.List.tail: empty list"
tail (Cons _ x) = x

-- Список без последнего элемента
init :: List a -> List a
init Nil = error "ITMOPrelude.List.init: empty list"
init (Cons a Nil) = Nil
init (Cons a l) = Cons a $ init l

-- Первый элемент
head :: List a -> a
head Nil = error "ITMOPrelude.List.head: empty list"
head (Cons a _) = a

-- Последний элемент
last :: List a -> a
last Nil = error "ITMOPrelude.List.last: empty list"
last (Cons a Nil) = a
last (Cons a l) = last l

-- n первых элементов списка
take :: Nat -> List a -> List a
take Zero _ = Nil
take _ Nil = error "ITMOPrelude.List.take: empty list"
take (Succ n) (Cons a l) = Cons a $ take n l

-- Список без n первых элементов
drop :: Nat -> List a -> List a
drop Zero l = l
drop (Succ n) Nil = error "ITMOPrelude.List.drop: empty list"
drop (Succ n) (Cons a l) = drop n l

-- Оставить в списке только элементы удовлетворяющие p
filter :: (a -> Bool) -> List a -> List a
filter p Nil = Nil
filter p (Cons a l) = case p a of
    True  -> Cons a $ filter p l
    False -> filter p l

-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter p = undefined

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile p Nil = Nil
takeWhile p (Cons a l) = case p a of
    True  -> Cons a $ takeWhile p l
    False -> Nil

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile p Nil = Nil
dropWhile p (Cons a l) = case p a of
    True  -> dropWhile p l
    False -> Cons a l

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span p Nil = Pair Nil Nil
span p (Cons a l) = case p a of
    True  -> Pair (Cons a $ fst $ span p l) (snd $ span p l)
    False -> Pair Nil $ Cons a l

-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break = undefined

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "ITMOPrelude.List.!!: empty list"
(Cons a _) !! Zero = a
(Cons a l) !! (Succ n) = l !! n

-- Список задом на перёд
reverse :: List a -> List a
reverse = undefined

-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons x xs) = undefined

-- (*) Все перестановки элементов данного списка
permutations :: List a -> List (List a)
permutations = undefined

-- (*) Если можете. Все перестановки элементов данного списка
-- другим способом
permutations' :: List a -> List (List a)
permutations' = undefined

-- Повторяет элемент бесконечное число раз
repeat :: a -> List a
repeat a = Cons a $ repeat a

-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl f z l = undefined

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl = undefined

-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> List a -> b
foldr f z l = undefined

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr = undefined

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map f l = undefined

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat = undefined

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap = undefined

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip a b = undefined

-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith = undefined
