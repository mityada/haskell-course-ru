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

append :: List a -> a -> List a
append Nil a = Cons a Nil
append (Cons a l) b = Cons a $ append l b

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
filter _ Nil = Nil
filter p (Cons a l) = case p a of
    True  -> Cons a $ filter p l
    False -> filter p l

-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter _ Nil = Nil
gfilter p (Cons a l) = case p a of
    Just b  -> Cons b $ gfilter p l
    Nothing -> gfilter p l

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile p (Cons a l) = case p a of
    True  -> Cons a $ takeWhile p l
    False -> Nil

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile p (Cons a l) = case p a of
    True  -> dropWhile p l
    False -> Cons a l

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span _ Nil = Pair Nil Nil
span p (Cons a l) = case p a of
    True  -> Pair (Cons a $ fst $ span p l) (snd $ span p l)
    False -> Pair Nil $ Cons a l

-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break p = span $ not . p

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "ITMOPrelude.List.!!: empty list"
(Cons a _) !! Zero = a
(Cons a l) !! (Succ n) = l !! n

-- Список задом на перёд
reverse :: List a -> List a
reverse Nil = Nil
reverse (Cons a l) = reverse l ++ Cons a Nil

-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons a l) = (scanl append (Cons a Nil) l) ++ subsequences l

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
foldl _ z Nil = z
foldl f z (Cons a l) = foldl f (f z a) l

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl _ z Nil = Cons z Nil
scanl f z (Cons a l) = Cons z $ scanl f (f z a) l

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
foldr _ z Nil = z
foldr f z (Cons a l) = f a $ foldr f z l

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr _ z Nil = Cons z Nil
scanr f z (Cons a l) = Cons (f a $ head x) x where
    x = scanr f z l

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (Cons a l) = Cons (f a) $ map f l

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat Nil = Nil
concat (Cons a l) = a ++ (concat l)

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap _ Nil = Nil
concatMap f (Cons a l) = f a ++ concatMap f l

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip _ Nil = Nil
zip Nil _ = Nil
zip (Cons a l1) (Cons b l2) = Cons (Pair a b) $ zip l1 l2

-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith _ _ Nil = Nil
zipWith _ Nil _ = Nil
zipWith f (Cons a l1) (Cons b l2) = Cons (f a b) $ zipWith f l1 l2
