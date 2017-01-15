{-# LANGUAGE DeriveGeneric #-}
module TPE.Types
 ( Case (..)
 , Plateau
 , IDP
 , Info'
 , Info
 , Gene
 , Genome
 , Population
 ) where

import TPE.Prelude

--  joueur      1   2
data Case = V | O | X
 deriving (Eq, Ord, Enum, Generic)

instance Show Case
  where
    show V = " "
    show O = "O"
    show X = "X"

instance Hashable Case

type Plateau = [Case]
type IDP = Int

type Info' = (Plateau, Vector IDP, Case, Case, Bool)
type Info = UVector IDP

type Gene = IDP
type Genome = UVector Gene
type Population = Vector Genome
