{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module TPE.Prelude
 ( module Exports
 , (.:)
 , occurrences
 , copy
 , idxs
 , zipCons
 , fromJustEx
 , ifMdo
 , iterateN
 , printLn
 ) where

import ClassyPrelude as Exports hiding (hashNub, ordNub)
import Control.Lens as Exports ((&), (^?!), (.~), view, _1)
import Control.Lens.At as Exports (ix)
import Data.List as Exports (iterate)
import Data.Bitraversable as Exports
import Control.Monad.Random as Exports hiding (fromList)
import Data.Witherable as Exports (hashNub, ordNub)
import Data.Vector (scanl')
import Data.ChunkedZip
import Control.Monad.Reader as Exports (reader)
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr 9 .:

occurrences :: (IsSequence a, Eq (Element a)) => Element a -> a -> Int
occurrences c = length . filter (== c)

copy :: a -> (a, a)
copy x = (x, x)

idxs :: Eq a => (a -> b -> b) -> b -> Vector a -> Vector b
idxs f s t = scanl' (\i (a,b) -> if a==b then i else f b i) (f (headEx t) s) . zipCons $ t

zipCons :: (Zip f, IsSequence (f a)) => f a -> f (a, a)
zipCons = uncurry zip . bimap toNullable tail . copy . impureNonNull

fromJustEx :: Maybe a -> a
fromJustEx = fromMaybe (error "TPE.Prelude.JustEx")

ifMdo :: Monad m => m Bool -> (a -> a) -> (a -> m a)
ifMdo b' f x = do
  b <- b'
  return $ if b then f x else x

iterateN :: Int -> (a -> a) -> a -> a
iterateN !n f !x
  | n <= 0    = x
  | otherwise = iterateN (n - 1) f (f x)

printLn :: (Show a) => a -> IO ()
printLn x = do
  print x
  putStr "\n"
