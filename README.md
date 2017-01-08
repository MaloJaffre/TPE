# TPE

add description of TPE here
cmp :: forall g. RandomGen g => Genome -> Genome -> Rand g Ordering
cmp a b = go LT 0 0
  where
    go :: Ordering -> Int -> Int -> Rand g Ordering
    go o i n = if
      | n == 60 -> return EQ
      | fini ^?! ix i -> return o
      | otherwise -> case o of
        LT -> do
          i' <- indice a i n
          go (alterne o) i' (n+1)
        GT -> do
          i' <- indice b i n
          go (alterne o) i' (n+1)

    alterne :: Ordering -> Ordering
    alterne GT = LT
    alterne LT = GT

    indice :: Genome -> IDP -> Int -> Rand g IDP
    indice g i n
      | n `mod` 20 == 0 = gen $ infos ^?! ix i
      | otherwise       = return $ g ^?! ix i
0 35
5 53
10 55
100 76
10000 82
