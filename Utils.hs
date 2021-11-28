module Utils where

import BigNumber (BigNumber,scanner, output,somaBN, subBN,mulBN,divBN,isEqual)

enesimo :: [a] -> BigNumber -> a -- !! version with big number as argument
enesimo x [1,0] = head x
enesimo x y = enesimo (tail x) (subBN y [0,1])

generateList :: BigNumber -> BigNumber -> [BigNumber] -- generates list of big numbers from x to y
generateList x y 
  | isEqual x y = [x]
  | otherwise= [x] ++ generateList (somaBN x [0,1]) y