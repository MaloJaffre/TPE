{-# LANGUAGE OverloadedLists #-}
module TPE.Plateau.Manips
 ( translat
 , symetrie
 , rotation
 , identiques
 , simp
 , reduce
 , coups
 , valide
 , joueur
 , gagnant
 , moving
 , moves
 , places
 , autre
 , accessCase
 , prettyp
 ) where

import TPE.Prelude
import TPE.Types

valide :: Plateau -> Bool
valide p = no < 4 && nx < 4 && no - nx < 2 && no - nx >= 0 &&
 length (gagnant p) < 2 && (joueur p `notElem` gagnant p || moving p)
  where
    no = occurrences O p
    nx = occurrences X p

gagnant :: Plateau -> [Case]
gagnant p = mapMaybe ($ p) [go O, go X]
  where
    go f = map headEx .
      find (\(a : b : c : _) -> a == f && a == b && b == c ) .
      (>>= (take 3 . iterate translat)) . take 2 . iterate rotation

coups :: Case -> Plateau -> Vector Plateau
coups f p = reduce $ if moving p then moves f p else places p

simp :: Plateau -> Plateau
simp = minimumEx . identiques

reduce :: Vector Plateau -> Vector Plateau
reduce = sort . hashNub . map simp . filter valide

symetrie  :: Plateau -> Plateau
symetrie (a : b : c :
         d : e : f :
         g : h : i : _)
         =
        [g , h , i ,
         d , e , f ,
         a , b , c ]

rotation :: Plateau -> Plateau
rotation (a : b : c :
         d : e : f :
         g : h : i : _)
         =
        [g , d , a ,
         h , e , b ,
         i , f , c ]

translat :: Plateau -> Plateau
translat (a : b : c :
         d : e : f :
         g : h : i : _)
         =
        [g , h , i ,
         a , b , c ,
         d , e , f ]

prettyp :: Plateau -> Text
prettyp (a : b : c :
         d : e : f :
         g : h : i : _)
         = unlines . map (pack . show) . asList $ [(a,b,c), (d,e,f), (g,h,i)]

moving :: Plateau -> Bool
moving p = occurrences V p == 3

joueur :: Plateau -> Case -- only if moving p == false
joueur p
 | no     == nx = O
 | no - 1 == nx = X
 | otherwise    = V
  where
   no = occurrences O p
   nx = occurrences X p

moves :: Case -> Plateau -> Vector Plateau
moves f p' = do
  p <- identiques p'
  (a, b) <- [(0, 1), (1, 0), (1, 4), (4, 1)]
  guard $ p ^?! ix a == V && p ^?! ix b == f
  return $ p & ix a .~ f & ix b .~ V

places :: Plateau -> Vector Plateau
places p = do
 i <- [0..8]
 guard $ p ^?! ix i == V
 return $ p & ix i .~ joueur p

identiques :: Plateau -> Vector Plateau
identiques p = fromList . (>>= (take 4 . iterate rotation)) $ [p, symetrie p]

autre :: Case -> Case
autre O = X
autre X = O

accessCase :: Case -> (a, a) -> a
accessCase O (a, _) = a
accessCase X (_, b) = b
