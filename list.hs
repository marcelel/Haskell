
{-# LANGUAGE InstanceSigs #-}
import Data.Char
data Tree a = Node a [Tree a] deriving Show

maketree :: Int -> Int -> a -> Tree a
maketree 0 _ v = Node v []
maketree d b v = Node v (fun d b v)

fun :: Int -> Int -> a -> [Tree a] 
fun depth branching val 
    | depth > 0 = replicate branching (Node val $ fun (depth - 1) branching val)
    | depth == 0 = []
    

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Node a list) = Node (f a) (map (fmap f) list)



data Lista a = Empty | Lista {headval :: a, tailval :: Lista a} deriving Show

map' :: (a -> b) -> Lista a -> Lista b
map' f Empty = Empty 
map' f (Lista v t) = Lista (f v) (map' f t)

fromNormalList :: [a] -> Lista a
-- fromNormalList = foldr Lista Empty
fromNormalList [] = Empty
fromNormalList (x:xs) = Lista x (fromNormalList xs)

foldr' :: (a -> b -> b) -> b -> Lista a -> b
foldr' f acc Empty = acc
foldr' f acc (Lista v t) = f v (foldr' f acc t)


foldl' :: (b -> a -> b) -> b -> Lista a -> b
foldl' f acc Empty = acc
foldl' f acc (Lista v t) = f (foldl' f acc t) v 

zipWith' :: (a -> b -> c) -> Lista a -> Lista b -> Lista c
zipWith' f Empty _ = Empty
zipWith' f _ Empty = Empty
zipWith' f (Lista a ta) (Lista b tb) = Lista (f a b) (zipWith' f ta tb)

infixr 5 .++
(.++) :: Lista a -> Lista a -> Lista a
(Lista a Empty) .++ b = Lista a b
(Lista a ta) .++ b = Lista a (ta .++ b)

toNormalList :: Lista a -> [a]
-- toNormalList Empty = []
-- toNormalList (Lista a t) = a : toNormalList t
toNormalList = foldr' (:) []

-- instance Functor Lista where
--     fmap :: (a -> b) -> Lista a -> Lista b
--     fmap f Empty = Empty
--     fmap f (Lista v t) = Lista (f v) (fmap f t)

-- instance Applicative Lista where
--     pure a = Lista a Empty
--     (<*>) :: Lista (a -> b) -> Lista a -> Lista b
--     Empty <*> lista = Empty
--     (Lista f ft) <*> lista = fmap f lista .++ (ft <*> lista) 

instance Monad Lista where 
    return a = Lista a Empty
    (>>=) :: Lista a -> (a -> Lista b) -> Lista b
    Empty >>= f = Empty
    (Lista v t) >>= f = f v .++ (t >>= f)