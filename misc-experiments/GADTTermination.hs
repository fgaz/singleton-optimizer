{-# LANGUAGE GADTs, TypeFamilies, DataKinds, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module GADTTermination where

data T a where
    I :: Int -> T Int
    -- P' :: T a -> T b -> T (a, b) -- for some reason this breaks LH

foo :: T Int -> Int
foo (I i) = i

data Expr t where
    C :: Int -> Expr Int
    P :: Expr a -> Expr b -> Expr (a, b)
    Plus :: Expr Int -> Expr Int -> Expr Int
    Fst :: Expr (a, b) -> Expr a

semExpr :: Expr a -> a
semExpr (C i) = i
semExpr (P x y) = (semExpr x, semExpr y)
semExpr (Plus x y) = semExpr x + semExpr y
semExpr (Fst x) = fst (semExpr x)

data Expr2 where
    C2 :: Int -> Expr2
    P2 :: Expr2  -> Expr2 -> Expr2
    Plus2 :: Expr2 -> Expr2 -> Expr2
    Fst2 :: Expr2 -> Expr2

data Value = VI Int | VP Value Value

semExpr2 :: Expr2 -> Value
semExpr2 (C2 i) = VI i
semExpr2 (P2 x y) = VP (semExpr2 x) (semExpr2 y)
semExpr2 (Plus2 x y) = case (semExpr2 x, semExpr2 y) of
                         (VI vx, VI vy) -> VI (vx + vy)
                         _              -> error "runtime type error"
semExpr2 (Fst2 x) = case semExpr2 x of
                      VP vx _vy -> vx
                      _         -> error "runtime type error"


----------------------------------

data Nat = Z | S Nat

-- 3 = S (S (S Z))

data Vec (n :: Nat) where
    V0 :: Vec 'Z
    Vn :: Char -> Vec n -> Vec ('S n)

type family NatPlus (n :: Nat)  (m :: Nat) where
    NatPlus n 'Z = n
    NatPlus n ('S m) = 'S (NatPlus n m)

v1 = Vn 'a' V0
--v2 = Vn 'b' v2
l1 = 1 : l1
append :: Vec n -> Vec m -> Vec (NatPlus n m)
append _xs _ys = undefined

bar :: Vec n -> Vec n -> ()
bar _ _ = undefined

data Teq (a :: Nat) (b :: Nat) where
    Refl :: Teq t t

proof1 :: Teq (NatPlus n 'Z) n
proof1 = Refl

proof :: Teq (NatPlus 'Z n) n
proof = undefined
-- proof = proof -- does not work
-- proof = let x = x in x -- this works >.< LH/#1404 (should be fixed in dev)
-- proof = undefined -- works too, but this time it's understandable

baz :: forall n . Vec n -> ()
baz xs = case proof :: Teq (NatPlus 'Z n) n of
           Refl -> bar xs (append V0 xs)

danger :: forall a b . Vec a -> Vec b
danger x = case undefined :: Teq a b of
    Refl -> x

data Peano2 = S2 Peano2 | Z2

total2 Z2 = Z2
total2 (S2 n) = total2 n

infinite = infinite'
  where infinite' = add2 infinite' Z2

add2 n Z2 = n
add2 n (S2 m) = S2 (add2 n m)

