module Units where

import FormalSet

type Dimension = Int
type Identifier = String

-- it would be more correct to have Dis require at least 2 elements (i.e. Dis Unit (Set Unit))
-- but this project is for fun
-- and also I would have to prove that the original unit was not part of the set
data Unit = Constant | Dis (Set Unit) | Prop Unit Unit | Symbol Identifier Dimension

-- todo: conversion using lookup table and brute force tree search
-- todo:
parseUnit :: String -> Maybe Unit

instance Semigroup Unit where
    (Dis xs) <> d@(Dis ys) = (sym <> d) <> (if isEmpty dis then Constant else Dis dis)
        where (sym, dis) = case (pop xs) of
                    (Just a, b) -> (a, b)
                    (Nothing, b) -> (Constant, Empty)
    (Prop a b) <> (Prop x y) = Prop (a <> x) (b <> y)
    a <> (Prop x y) = Prop (a <> x) y

    -- sift through `us` by popping, then incrementing if encountered same symbol
    s@(Symbol ident pwr) <> (Dis us) = Dis $ if done then newSet else s +> us
        where (newSet, done) = helper us Empty False
              helper :: Set Unit -> Set Unit -> Bool -> (Set Unit, Bool)
              helper from to True = (from `junct` to, True)
              helper Empty to _ = (to, False)
              helper from to _ = case e of
                    s@(Symbol i p) -> if ident == i then
                        helper rest (Symbol ident (p+pwr) +> to) True else
                        helper rest (s +> to) False
                    _ -> error "Semigroup => Unit: `simplify' before using (<>)" -- sorry I'm lazy
                where (e, rest) = case pop from of
                                    (Just a, r) -> (a, r)
                                    (Nothing, r) -> (Constant, r)
    a@(Symbol _ _) <> b@(Symbol _ _) = a <> Dis (b +> Empty)
    Constant <> x = x
    x <> y = y <> x

-- somehow guarantee that units are passed through this function
createUnit :: Unit -> Unit
createUnit = simplify

-- todo use an alternative junct that adds units together in the set
simplify :: Unit -> Unit
simplify set | bottom == Empty = Dis top
             | otherwise = Prop (Dis top) (Dis bottom)
    where   getDis :: Bool -> Unit -> Set Unit
            getDis same (Dis Empty) = Empty
            getDis same (Dis Universal) = error "Units.simplify: units cannot have Universal"
            getDis same (Dis set) = foldr junct Empty . fmap (getDis same) $ set
            getDis same (Prop t b) = (getDis same t) `junct` (getDis (not same) b)
            getDis same x = if same then x+>Empty else Empty
            top = getDis True set
            bottom = getDis False set

-- assume they are simplified
instance Eq Unit where
    -- structural mismatch (since they are simplified)
    (Dis _) == (Prop _ _) = False
    (Dis _) == (Symbol _ _) = False
    (Symbol _ _) == (Prop _ _) = False
    (Dis xs) == (Dis ys) = xs == ys
    (Prop a b) == (Prop x y) = a == x && b == y
    (Symbol a b) == (Symbol x y) = a == x && b == y

instance Show Unit where
    show x = case x of
        (Symbol s d) -> if d == 1 then s else s ++ "^" ++ p1 ++ show d ++ p2
            where   p1 = if d < 0 then "(" else ""
                    p2 = if d < 0 then ")" else ""
        (Prop x y) -> show (simplify x) ++ "/" ++ show (simplify y)
        (Dis Universal) -> error "Units.Unit.show: cannot show a universal set"
        (Dis Empty) -> ""
        (Dis (Cons x xs)) -> if xs == Empty
                             then show x
                             else show x ++ "*" ++ show (Dis xs)

--    where userDefinedSymbol = case lookup x (map (\(s,u) -> (u, s)) globalSymbols) of
--        Just r -> r
--        Nothing -> "<Don't know how to show this unit>"

type UnitType = Float
data Measurement = Measurement UnitType Unit
instance Show Measurement where
    show (Measurement x u) = show x ++ show u

instance Num Measurement where
    (Measurement x u1) + (Measurement y u2) | u1 /= u2 = error (show u1 ++ " does not match " ++ show u2)
                                            | otherwise = Measurement (x + y) u1
    negate (Measurement x u1) = Measurement (negate x) u1
    (Measurement x u1) * (Measurement y u2) = Measurement (x * y) $ if u1 /= u2 then u1<>u2 else u1
    abs (Measurement x u1) = Measurement (abs x) u1
    signum (Measurement x u1) = (Measurement (signum x) Constant)
    fromInteger x = Measurement (fromInteger x) Constant


globalSymbols = [
    ("N", Prop (Dis (fromList [Symbol "kg" 1, Symbol "m" 1])) (Symbol "s" 2))
    ] :: [(Identifier, Unit)]