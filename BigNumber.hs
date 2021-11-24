
module BigNumber
  ( BigNumber, -- exportar o tipo
    scanner,
    output,
    somaBN, -- e as operações
    subBN,
    mulBN,
    divBN, 
    isEqual
  )
where

import Data.Char
type BigNumber = [Int]

--the first number of any BigNumber symbolizes its signal 0- positive 1-negative

scanner :: String -> BigNumber
scanner arg = [digitToInt x | x <- arg]

output :: BigNumber -> String
output arg = [intToDigit x | x <- arg]

inverso :: BigNumber -> BigNumber
inverso x
  | take 1 x == [1] = [0] ++ drop 1 x
  | otherwise = [1] ++ drop 1 x

valorAbsoluto :: BigNumber -> BigNumber
valorAbsoluto x = [0] ++ drop 1 x

--
zeroUntil :: [Int] -> [Int]
zeroUntil [] = []
zeroUntil [0] = [0]
zeroUntil (0 : xs) = zeroUntil xs
zeroUntil (x : xs)
  | x > 0 = x : xs 
  | otherwise = x : zeroUntil xs 

isBigger :: BigNumber -> BigNumber -> Bool
isBigger [] [] = False
isBigger (x : xs) (y : ys)
  | length xs < length ys = False
  | length xs > length ys = True
  | x > y = True
  | x < y = False
  | otherwise = isBigger xs ys

isEqual :: BigNumber -> BigNumber -> Bool
isEqual [] [] = True
isEqual x [] = False
isEqual [] y = False
isEqual (x : xs) (y : ys)
  | x /= y = False
  | otherwise = isEqual xs ys

somaAux :: BigNumber -> BigNumber -> BigNumber
somaAux [] [] = []
somaAux (x : xs) (y : ys)
  | x + y >= 10 = [(x + y) `mod` 10] ++ somaAux xs_mod ys
  | otherwise = [(x + y) `mod` 10] ++ somaAux xs ys
  where
    xs_mod = [head xs + 1] ++ drop 1 xs

somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN x y
  | head x == head y = signal ++ zeroUntil (reverse (somaAux (reverse bigger_list) (reverse smaller_list)))
  | otherwise = signal ++ zeroUntil (reverse (subAux (reverse bigger_list) (reverse smaller_list)))
  where
    bigger_list
      | isBigger (drop 1 x) (drop 1 y) = replicate ((max (length y - length x) 0) + 1) 0 ++ drop 1 x --removing the signal here
      | otherwise = replicate ((max (length x - length y) 0) + 1) 0 ++ drop 1 y
    smaller_list
      | isBigger (drop 1 x) (drop 1 y) = replicate ((max (length x - length y) 0) + 1) 0 ++ drop 1 y --removing the signal here
      | otherwise = replicate ((max (length y - length x) 0) + 1) 0 ++ drop 1 x
    signal
      | isBigger (drop 1 x) (drop 1 y) = take 1 x
      | otherwise = take 1 y

subAux :: BigNumber -> BigNumber -> BigNumber
subAux [] [] = []
subAux (x : xs) (y : ys)
  | x - y < 0 = [(x - y) + 10] ++ subAux xs_mod ys
  | otherwise = [(x - y)] ++ subAux xs ys
  where
    xs_mod = [head xs - 1] ++ drop 1 xs

subBN :: BigNumber -> BigNumber -> BigNumber
subBN x y = somaBN x (inverso y)

mulAux :: Int -> Int -> BigNumber -> BigNumber
mulAux 0 _ [] = []
mulAux carry _ [] = [carry]
mulAux carry x (y : ys) = mulAux ((y * x) `div` 10) x ys ++ [(y * x) `mod` 10 + carry]

mulFatores :: Int -> BigNumber -> BigNumber -> [BigNumber]
mulFatores _ [] _ = []
mulFatores offset (x : xs) y = [[0] ++ (mulAux 0 x (reverse y)) ++ replicate offset 0] ++ mulFatores (offset + 1) xs y

somaRec :: [BigNumber] -> BigNumber
somaRec [] = [0, 0]
somaRec (x : xs) = somaBN x (somaRec xs)

mulBN :: BigNumber -> BigNumber -> BigNumber
mulBN x y = signal ++ drop 1 (somaRec (mulFatores 0 (reverse x_no_signal) y_no_signal))
  where
    x_no_signal = drop 1 x
    y_no_signal = drop 1 y
    signal
      | head x == head y = [0]
      | otherwise = [1]

bigNumberDiv :: BigNumber -> BigNumber -> BigNumber -> [BigNumber]
bigNumberDiv try x y
  | isBigger (drop 1 (mulBN y try)) (drop 1 x) = [subBN try [0, 1], subBN x (mulBN y (subBN try [0, 1]))]
  | otherwise = bigNumberDiv (somaBN try [0, 1]) x y

accumulateUntilBiggerOrEqual :: Int -> BigNumber -> BigNumber -> BigNumber
accumulateUntilBiggerOrEqual acc x y
  | isBigger (take acc x) y || isEqual (take acc x) y = take acc x
  | otherwise = accumulateUntilBiggerOrEqual (acc + 1) x y

divAux :: BigNumber -> BigNumber -> [Int] -> [BigNumber]
divAux x y final
  | isBigger y x = [final, drop 1 x]
  | otherwise = divAux (last partial_results ++ drop (length partial_quo) x) y (final ++ drop 1 (head partial_results))
  where
    partial_results = bigNumberDiv [1, 0] partial_quo y
    partial_quo = accumulateUntilBiggerOrEqual 1 x y

divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)
divBN x y = (signal_quotient ++ head results_without_sign, signal_remainder ++ last results_without_sign)
  where
    results_without_sign = divAux (valorAbsoluto x) (valorAbsoluto y) []
    signal_quotient
      | head x == head y = [0]
      | otherwise = [1]
    signal_remainder = take 1 x

safeDivBN :: BigNumber -> BigNumber -> Maybe (BigNumber, BigNumber)
safeDivBN x y 
   | isEqual y [0,0] || isEqual y [1,0] = Nothing
   | otherwise = Just (divBN x y)
