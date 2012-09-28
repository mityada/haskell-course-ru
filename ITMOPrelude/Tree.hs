module ITMOPrelude.Tree where

import Prelude (Show,Read,error)

data Tree a = Nil | Node { left :: Tree a, right :: Tree a, key :: a } deriving (Show,Read)

emptyTree = Nil

addRoot :: Tree a -> a -> Tree a
addRoot t a = Node t Nil a

addLeft :: Tree a -> a -> Tree a
addLeft Nil a = Node Nil Nil a
addLeft (Node l r k) a = Node (addLeft l a) r k

addRight :: Tree a -> a -> Tree a
addRight Nil a = Node Nil Nil a
addRight (Node l r k) a = Node l (addRight r a) k

rotateLeft :: Tree a -> Tree a
rotateLeft (Node a (Node b c x) p) = Node (Node a b p) c x

rotateRight :: Tree a -> Tree a
rotateRight (Node (Node a b x) c p) = Node a (Node b c p) x

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Nil = Nil
mapTree f (Node l r k) = Node (mapTree f l) (mapTree f r) (f k)

foldTree :: (b -> b -> a -> b) -> b -> Tree a -> b
foldTree _ z Nil = z
foldTree f z (Node l r k) = f (foldTree f z l) (foldTree f z r) k
