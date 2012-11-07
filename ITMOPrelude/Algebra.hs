{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPredule.Algebra where

-- Реализовать для всего,
-- что только можно из
import ITMOPrelude.Primitive
-- всевозможные инстансы для классов ниже 

-- Если не страшно, то реализуйте их и для
import ITMOPrelude.List
import ITMOPrelude.Tree

-- Классы
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

class Monoid a => Group a where
    ginv :: a -> a

newtype NatSum = NatSum { getNatSum :: Nat }
newtype NatProduct = NatProduct { getNatProduct :: Nat }

newtype IntSum = IntSum { getIntSum :: Int }
newtype IntProduct = IntProduct { getIntProduct :: Int }

newtype RatSum = RatSum { getRatSum :: Rat }
newtype RatProduct = RatProduct { getRatProduct :: Rat }

-- Инстансы писать сюда

instance Monoid NatSum where
    mempty = NatSum natZero 
    mappend (NatSum a) (NatSum b) = NatSum $ a +. b

instance Monoid NatProduct where
    mempty = NatProduct natOne
    mappend (NatProduct a) (NatProduct b) = NatProduct $ a *. b

instance Monoid IntSum where
    mempty = IntSum intZero
    mappend (IntSum a) (IntSum b) = IntSum $ a .+. b

instance Monoid IntProduct where
    mempty = IntProduct intOne
    mappend (IntProduct a) (IntProduct b) = IntProduct $ a .*. b

instance Monoid RatSum where
    mempty = RatSum $ Rat intZero natOne
    mappend (RatSum a) (RatSum b) = RatSum $ a %+ b

instance Monoid RatProduct where
    mempty = RatProduct $ Rat intOne natOne
    mappend (RatProduct a) (RatProduct b) = RatProduct $ a %* b

instance Group IntSum where
    ginv (IntSum a) = IntSum $ intNeg a

instance Group RatSum where
    ginv (RatSum a) = RatSum $ ratNeg a 

instance Group RatProduct where
    ginv (RatProduct a) = RatProduct $ ratInv a
