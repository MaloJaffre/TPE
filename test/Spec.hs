{-# OPTIONS_GHC -F -pgmF htfpp #-}
import Test.Framework
import Test.Framework.TestTypes
import TPE

main :: IO ()
main = htfMain htf_thisModulesTests

test_1 :: Bool
test_1 =  all ($ infos)
  [ all (\(_, _, p, w, m) -> p/=w||m), --déjà gagné |< 2 tours avant
    all (\(_, n, _, w, _) -> not $ null n && w==V) -- pas de feuilles nulles
  ]

instance AssertionWithTestOptions Bool
  where
    testOptions = const defaultTestOptions
    assertion = assertBool
