{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Lib
    where

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
type Behavior a = [Time] -> [a]
type Event a = [Time] -> [Maybe a]

at :: Behavior a -> [Time] -> a
at behavior times = last (behavior times)

occ :: forall a . Event a -> [Time] -> [(Time, a)]
occ event times = foldr combiner [] timedEvents
  where
    timedEvents :: [(Time, Maybe a)]
    timedEvents = zip times $ event times

    combiner :: (Time, Maybe a) -> [(Time, a)] -> [(Time, a)]
    combiner (_, Nothing) accum = accum
    combiner (t, Just a) accum  = (t, a) : accum

five :: Behavior Float
five times = map (const 5) times

yotime :: Behavior Time
yotime x = x

eventAt10syo :: Event Int
eventAt10syo times = foldr f [] times
  where
    f :: Time -> [Maybe Int] -> [Maybe Int]
    f t accum
        | truncate t `mod` 10 == 0 = Just (10 * truncate t) : accum
        | otherwise                = Nothing                : accum


lift0 :: a -> Behavior a
lift0 x times = map (\t -> x) times

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
