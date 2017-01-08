{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
module TPE.Gen
 ( Gen(..)
 , shuffle
 ) where

import TPE.Prelude
import TPE.Plateau
import TPE.Types
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as V
class Gen i a  where
  gen :: RandomGen g => i -> Rand g a
instance Gen () Genome where
  gen :: RandomGen g => () -> Rand g Genome
  gen = const . map (fromList . toList) . mapM gen $ infos
instance Gen Info Gene where
    gen :: RandomGen g => Info -> Rand g Gene
    gen is = if null is then return 0 else do
      i <- liftRand $ randomR (0, length is -1)
      return $ is ^?! ix i

instance Gen () Bool where
  gen :: RandomGen g => () -> Rand g Bool
  gen = const . liftRand $ random

-- Knuth-Fisher-Yates shuffle https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
shuffle :: forall a g. RandomGen g => Vector a -> Rand g (Vector a)
shuffle a = liftRand $ \g -> runST $ (>>= bitraverse V.freeze return) . (>>= modi (length a)) . bisequence . (, return g) . V.thaw $ a
  where
    modi :: Int -> (V.MVector s a, g) -> ST s (V.MVector s a, g)
    modi l (a', g) = do
      r <- newSTRef g
      let
        rs bs = map fst . bitraverse return (writeSTRef r) <=< map (randomR bs) $ readSTRef r
      forM_ [0..l - 2] $ \i -> do
        j <- rs (i, l - 1)
        V.swap a' i j
      sequence (a', readSTRef r)
