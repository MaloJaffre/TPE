{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module TPE
 ( module Exports
 , someFunc
 ) where

import TPE.Prelude as Exports
import TPE.Plateau as Exports
import TPE.Plateau.Manips as Exports
import TPE.Types as Exports
import TPE.Gen as Exports

-- | Prints someFunc
--
-- >>> someFunc 10
-- someFunc
someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: Text)
