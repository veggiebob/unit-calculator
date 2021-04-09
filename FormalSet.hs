{-
(copied from ../math/FormalSet.hs)
-}


module FormalSet where
import Data.List (intercalate)

class Poppable c where
    pop :: c a -> (Maybe a, c a)
    popChain :: Integer -> c a -> ([a], c a)
    popChain 0 xs = ([], xs)
    popChain n xs | n < 0 = error "cannot pop negative amount"
                  | otherwise = case pop xs of
                     (Nothing, col) -> ([], xs)
                     (Just e, col) -> (e:list, rest)
                     where (list, rest) = popChain (n-1) xs

data Set a = Universal | Empty | Cons a (Set a)

instance Poppable Set where
    pop Empty = (Nothing, Empty)
    pop (Cons x set) = (Just x, set)

toList :: Set a -> [a]
toList Universal = error "FormalSet.toList: Universal set cannot be converted to list"
toList Empty = []
toList (Cons x set) = x:toList set

fromList :: Eq a => [a] -> Set a
fromList [] = Empty
fromList (x:xs) | set `contains` x = set
                | otherwise = Cons x set
                where set = fromList xs

size :: Set a -> Int
size Empty = 0
size Universal = error "FormalSet.size: cannot evaluate size of Universal set"
size (Cons x xs) = 1 + size xs

contains :: Eq a => Set a -> a -> Bool
Universal `contains` _ = True
Empty `contains` _ = False
(Cons x set) `contains` y = x == y || set `contains` y

junct :: Eq a => Set a -> Set a -> Set a
Universal `junct` _ = Universal
_ `junct` Universal = Universal
Empty `junct` x = x
x `junct` Empty = x
set `junct` (Cons y ys) | set `contains` y = set `junct` ys
                        | otherwise = (Cons y set) `junct` ys

subset :: Eq a => Set a -> Set a -> Bool
Empty `subset` _ = True
_ `subset` Empty = False
_ `subset` Universal = True
(Cons x xs) `subset` set = set `contains` x && xs `subset` set

(+>) :: Eq a => a -> Set a -> Set a
a +> b = (Cons a Empty) `junct` b

isEmpty :: Set a -> Bool
isEmpty Empty = True
isEmpty _ = False

instance Eq a => Eq (Set a) where
    x == y = x `subset` y && y `subset` x

instance Show a => Show (Set a) where
    show Universal = "U"
    show Empty = "{}"
    show set = "{" ++ (intercalate ", " . map show $ toList set) ++ "}"

instance Functor Set where
    fmap _ Empty = Empty
    fmap _ Universal = Universal
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable Set where
    foldr _ _ Universal = error "FormalSet.foldr: cannot fold a Universal set"
    foldr _ i Empty = i
    foldr f i (Cons x xs) = foldr f (f x i) xs
