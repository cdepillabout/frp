{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Streams where

import BasicPrelude hiding (show)

import Control.Category
import Data.Functor.Foldable (Fix(..))
import Data.Typeable
import Text.Show

newtype Lala a f = Lala { unLala :: f }

newtype StreamF g a f = SConsF { runStream :: g a f }

type Stream g a = Fix (StreamF g a)

data ListF a f = Nil | ConsF a f

type List a = Fix (ListF a)

constStream :: forall a g . a -> (forall x y . x -> y -> g x y) -> Stream g a
constStream a combiner = Fix . SConsF . combiner a $ constStream a combiner

takeFromStream :: forall g a
                . Int
               -> (forall x f . g x f -> x)
               -> (forall x f . g x f -> f)
               -> Stream g a -> [a]
takeFromStream n getA getF stream =
    go n stream []
  where
    go :: Int -> Stream g a -> [a] -> [a]
    go i (Fix (SConsF g)) accum
        | i <= 0    = accum
        | otherwise = go (i - 1) (getF g) (getA g : accum)

streamToList :: forall g a
              . (forall x f . g x f -> x)
             -> (forall x f . g x f -> f)
             -> Stream g a -> [a]
streamToList getA getF (Fix (SConsF g)) = getA g : streamToList getA getF (getF g)

streamFrom :: forall g a b
            . a
           -> (a -> a)
           -> (a -> b)
           -> (forall y . b -> y -> g b y)
           -> Stream g b
streamFrom a next toStreamVar combiner = Fix . SConsF . combiner (toStreamVar a) $ streamFrom (next a) next toStreamVar combiner

newtype AutoF' p a b f = AConsF' { runAuto' :: p a (b, f) }

type AutoF = AutoF' (->)

type Auto a b = Fix (AutoF a b)

newtype Buto a b = BCons { runButo' :: a -> (b, Buto a b) }

instance (Typeable a, Typeable b) => Show (AutoF' (->) a b f) where
    show :: AutoF' (->) a b f -> String
    show _ = "{ Auto ("
          <> show (typeRep (Proxy :: Proxy a))
          <> ") ("
          <> show (typeRep (Proxy :: Proxy b))
          <> ") }"

instance Category Buto where
    id :: Buto a a
    id = BCons $ \a -> (a, undefined)

    (.) :: Buto b c -> Buto a b -> Buto a c
    (.) (BCons butoBC) (BCons butoAB) = BCons $ \a ->
        let (b, newButoAB) = butoAB a
            (c, newButoBC) = butoBC b
        in (c, newButoBC . newButoAB)

composeAuto :: Auto a b -> Auto b c -> Auto a c
composeAuto autoAB autoBC = aconsF $ \a ->
    let (b, newAutoAB) = runAuto autoAB a
        (c, newAutoBC) = runAuto autoBC b
    in (c, composeAuto newAutoAB newAutoBC)

runAuto :: Auto a b -> a -> (b, Auto a b)
runAuto (Fix (AConsF' f)) = f

aconsF :: (a -> (b, Auto a b)) -> Auto a b
aconsF = Fix . AConsF'

myStreamAuto :: Auto a Int
myStreamAuto = streamFrom 1
  where
    streamFrom :: Int -> Auto a Int
    streamFrom n = aconsF $ const (n, streamFrom (n+1))

autoToList :: a -> Auto a a -> [a]
autoToList start (Fix (AConsF' f)) = let (newA, newAuto) = f start
                                     in newA : autoToList newA newAuto


settableAuto :: Auto (Maybe Int) Int
settableAuto = counterFrom 1
  where
    counterFrom :: Int -> Auto (Maybe Int) Int
    counterFrom n = aconsF $ \case
        Nothing -> (n, counterFrom (n + 1))
        Just x  -> (x, counterFrom (x + 1))


testAuto' :: Auto a b -> [a] -> [b]
testAuto' auto = fst . testAuto auto

testAuto :: forall t a b . Foldable t => Auto a b -> t a -> ([b], Auto a b)
testAuto auto = foldl f ([], auto)
  where
    f :: ([b], Auto a b) -> a -> ([b], Auto a b)
    f (accum, auto) a =
        let (newB, newAuto) = runAuto auto a
        in (accum <> [newB], newAuto)


summer :: Monoid a => Auto a a
summer = sumFrom mempty
  where
    sumFrom :: Monoid a => a -> Auto a a
    sumFrom n = aconsF $ \input ->
        let s = n <> input
        in  ( s , sumFrom s )
