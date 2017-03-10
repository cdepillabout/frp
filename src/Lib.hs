{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Lib
    where

import Control.Applicative
import Control.Monad

someFunc :: IO ()
someFunc = putStrLn "someFunc"


divide :: Int -> Int -> Maybe Int
divide x 0 = Nothing
divide x y = Just (x `div` y)


foo :: Int -> Int -> Int
foo a b = case divide a b of
              Nothing -> -9999
              Just x -> x


type Time = Float
newtype Behavior a = Behavior { unBehavior :: [Time] -> [a] } deriving Functor
newtype Event a = Event { unEvent :: [Time] -> [Maybe a] } deriving Functor

at :: Behavior a -> [Time] -> a
at (Behavior behavior) times = last (behavior times)

occ :: forall a . Event a -> [Time] -> [(Time, a)]
occ (Event event) times = foldr combiner [] timedEvents
  where
    timedEvents :: [(Time, Maybe a)]
    timedEvents = zip times $ event times

    combiner :: (Time, Maybe a) -> [(Time, a)] -> [(Time, a)]
    combiner (_, Nothing) accum = accum
    combiner (t, Just a) accum  = (t, a) : accum

five :: Behavior Float
five = Behavior $ \time -> map (const 5) time

time :: Behavior Time
time = Behavior id

eventAt10syo :: Event Int
eventAt10syo = Event $ \times -> foldr f [] times
  where
    f :: Time -> [Maybe Int] -> [Maybe Int]
    f t accum
        | truncate t `mod` 10 == 0 = Just (10 * truncate t) : accum
        | otherwise                = Nothing                : accum


instance Applicative Behavior where
    pure :: a -> Behavior a
    pure a = Behavior $ fmap (const a)

    (<*>) :: Behavior (a -> b) -> Behavior a -> Behavior b
    (Behavior f) <*> (Behavior a) = Behavior $ \ts -> f ts <*> a ts


($*) :: Behavior (a -> b) -> Behavior a -> Behavior b
($*) = (<*>)

lift0 :: a -> Behavior a
lift0 = pure

lift1 :: (a -> b) -> Behavior a -> Behavior b
lift1 = fmap

lift2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
lift2 = liftA2

instance Num a => Num (Behavior a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance (Fractional a) => Fractional (Behavior a) where
    fromRational = pure . fromRational
    (/) = liftA2 (/)

instance Floating a => Floating (Behavior a) where
    pi = pure pi
    exp = fmap exp
    log = fmap log
    sqrt = fmap sqrt
    (**) = liftA2 (**)
    logBase = liftA2 logBase
    sin = fmap sin
    cos = fmap cos
    tan = fmap tan
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    tanh = fmap tanh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh


integral :: Behavior Float -> Behavior Float
integral (Behavior behavior) =
    Behavior $ \ts@(t:ts') -> reverse . third . foldr f (t, 0, [0]) $ zip ts' (behavior ts)
  where
    f :: (Float, Time) -> (Time, Float, [Float]) -> (Time, Float, [Float])
    f (t1, a) (t0, acc, list) =
        let acc' = acc + (t1 - t0) * a
        in (t1, acc', acc' : list)

    third :: (a, b, c) -> c
    third (a, b, c) = c

(.|.) :: Event a -> Event a -> Event a
Event a .|. Event b = Event $ \ts -> zipWith (<|>) (a ts) (b ts)

(==>) :: Event a -> (a -> b) -> Event b
(==>) = flip fmap

(-=>) :: Event a -> a -> Event a
eventA -=> b = eventA ==> const b

------------------
-- Church Stuff --
------------------


-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr :: (a -> [b] -> [b]) -> [b] -> [a] -> [b]
-- foldr :: (a -> String -> String) -> String -> [a] -> String
-- foldr :: (String -> String -> String) -> String -> [String] -> String

-- f :: a -> a
-- f = (\a -> a)

zero :: (a -> a) -> a -> a
zero = (\f x -> x)

one :: (a -> a) -> a -> a
one = (\f x -> f x)

two :: (a -> a) -> a -> a
-- two = (\f x -> f (f x))
two f x = f (f x)

-- two (\y -> y + 1) 0 == 2
-- two (\y -> 10) 0 == 10

-- data List a = Nil | Cons a (List a)

data ChurchList a = ChurchList (forall r . (a -> r -> r) -> r -> r)

-- evalChurchList :: (Int -> String -> String) -> String -> ChurchList Int -> String
evalChurchList :: (a -> r -> r) -> r -> ChurchList a -> r
evalChurchList combiner start (ChurchList f) = f combiner start

churchListToList :: ChurchList a -> [a]
churchListToList (ChurchList f) = f (\x acc -> x : acc) []

listToChurchList :: [a] -> ChurchList a
listToChurchList [] = ChurchList (\combiner start -> start )
listToChurchList (x:xs) = x `cons` listToChurchList xs

cons :: a -> ChurchList a -> ChurchList a
cons a (ChurchList f) = ChurchList (\combiner start -> combiner a (f combiner start))

----------------
-- List Stuff --
----------------

data MyType a = MyType { unMyType :: [Float] -> [a] }

instance Functor MyType where
    fmap :: (a -> b) -> MyType a -> MyType b
    fmap f (MyType mytype) = MyType $ \floats -> fmap f $ mytype floats

instance Applicative MyType where
    pure :: a -> MyType a
    pure x = MyType $ \floats -> fmap (const x) floats

    (<*>) :: MyType (a -> b) -> MyType a -> MyType b
    (MyType fs) <*> (MyType as) = MyType $ \floats -> zipWith ($) (fs floats) (as floats)

instance Monad MyType where
    return = pure

    (>>=) :: MyType a -> (a -> MyType b) -> MyType b
    (MyType ma) >>= k = MyType $ \floats -> join $ fmap (\a -> unMyType (k a) floats) (ma floats)
