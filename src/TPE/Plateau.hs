{-# LANGUAGE OverloadedLists #-}
module TPE.Plateau
 ( plateaux
 , symetrie
 , rotation
 , identiques
 , simp
 , vide
 , idsimp
 , infos'
 , infos
 , fini
 ) where

import TPE.Prelude
import TPE.Types
import TPE.Plateau.Manips
import qualified Data.Vector.Unboxed as U

plateaux :: Vector Plateau
plateaux = sortOn simp . filter valide . replicateM 9 $ fromList [V .. X]

-- HASH8120 -> (ID827 O, ID827 X)
idsimp' :: IntMap (IDP, IDP)
idsimp' = mapFromList . toList . zip (hash <$> plateaux) .
  idxs (\p (_, b) -> if moving p then (b+1,b+2) else (b+1,b+1) ) (-1, -1) . map simp $ plateaux

idsimp :: Plateau -> (IDP, IDP)
idsimp = fromJustEx . (`lookup` idsimp') . hash

-- ID827
infos' :: Vector Info'
infos' = map doIt . foldMap (\p -> if moving p then [(O,p), (X,p)] else [(joueur p, p)]) . reduce $ plateaux
  where
    doIt (c,p) =
      ( p
      , accessCase (autre c) . idsimp <$> coups c p
      , c
      , fromMaybe V . headMay . gagnant $ p
      , moving p)

infos :: Vector Info
infos = map (\(_, l, _, ws, _)-> if ws /= V then [] else U.convert l) infos'

fini :: UVector Bool
fini = fromList . toList . map null $ infos

vide :: Plateau
vide = [X, O] ++ replicate 5 V ++ [X, O]
