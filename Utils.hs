module Utils where

import BigNumber (BigNumber,scanner, output,somaBN, subBN,mulBN,divBN,isEqual)

enesimo :: [a] -> BigNumber -> a
enesimo x [1,0] = head x
enesimo x y = enesimo (tail x) (subBN y [0,1])

generateList :: BigNumber -> BigNumber -> [BigNumber]
generateList x y 
  | isEqual x y = [x]
  | otherwise= [x] ++ generateList (somaBN x [0,1]) y