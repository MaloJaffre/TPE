{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import TPE
import Prelude (readLn)
import qualified Data.Vector.Unboxed as U

cmp :: Genome -> Genome -> Ordering
cmp a b = go LT 0 0
  where
    go :: Ordering -> Int -> Int -> Ordering
    go !o !i !n = if
      | n == 60 -> EQ
      | fini ^?! ix i -> o
      | otherwise -> case o of
        LT -> go (alterne o) (a ^?! ix i) (n+1)
        GT -> go (alterne o) (b ^?! ix i) (n+1)

    alterne :: Ordering -> Ordering
    alterne GT = LT
    alterne LT = GT

mutate' :: RandomGen g => Genome -> Rand g Genome
mutate' g = do
  i <- liftRand $ randomR (0, length g - 1)
  n <- gen $ infos ^?! ix i
  return $ g & ix i .~ n

mutate :: RandomGen g => Population -> Rand g Population
mutate = traverse mutate'

crossingOver :: RandomGen g => Genome -> Genome -> Rand g (Genome, Genome)
crossingOver = map U.unzip .: (U.zipWithM . curry $ ifMdo (gen ()) swap)

reproduce :: RandomGen g => Population -> Rand g Population
reproduce p = map (>>= (\(a, b) -> fromList [a, b])) . mapM (uncurry crossingOver) . cons (headEx p, lastEx p) . zipCons $ p

step :: forall g. RandomGen g => Seq Population -> Rand g (Seq Population)
step ps = do
  new <- new'
  return $ new `cons` initEx ps
  where
    new' = shuffle <=< mutate <=< reproduce <=< shuffle . (select . zip p) . evaluate evs $ p
    (p, evs) = fromJustEx $ uncons ps

evaluate :: Seq Population -> Population-> Vector Int
evaluate evs p = foldl' (\m ev ->
  let r1 = (fromEnum . (==GT)) <$> zipWith cmp p ev in
  let r2 = (fromEnum . (==LT)) <$> zipWith cmp ev p in
  zipWith3 (\a b c -> a+b+c) m r1 r2) (0 <$ p) evs

select :: Vector (Genome, Int) -> Population
select = map fst . take 500 . sortOn (Down . snd)

main :: IO ()
main = do
  n <- readLn :: IO Int
  -- ind <- evalRandIO $ gen ()
  popinitiale <- evalRandIO . replicateM 10 . replicateM 1000 $ gen () -- :: IO (Seq Population)
  --pop <- evalRandIO $ step popinitiale
  popap <- evalRandIO $ (return . sortOn (Down . snd) . uncurry zip . bimap headEx (uncurry (flip evaluate) . fromJustEx . uncons) . copy) <=< iterateN n (>>= step) $ return popinitiale
  --c <- evalRandIO $ cmp a b
  -- d <- evalRandIO . mapM (uncurry cmp) . zipCons $ popinitiale
  -- print . map snd $ popap
  putStrLn . unlines . map (pack . show) $ popap
  printLn . stat . fst . headEx $ popap
  putStrLn . ppg . fst . headEx $ popap

ppg :: Genome -> Text
ppg = unlines . map (uncurry (++) . bimap (prettyp . view _1) (prettyp . view _1 . (\i -> infos' ^?! ix i))) . zip infos' . U.convert

stat :: Genome -> Int
stat = length . filter (\i -> fini ^?! ix i) . asVector . U.convert
