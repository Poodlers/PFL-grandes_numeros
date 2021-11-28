{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module BigNumber
  ( BigNumber, -- exportar o tipo
    scanner,
    output,
    somaBN, -- e as operações
    subBN,
    mulBN,
    divBN,
    isEqual,
  )
where

import Data.Char ( digitToInt, intToDigit )

type BigNumber = [Int]

--the first number of any BigNumber symbolizes its signal 0- positive 1-negative

scanner :: String -> BigNumber
scanner (s:xs) |  s == '-' = 1 : [digitToInt x | x <- xs]
              | s == '+' = 0 : [digitToInt x | x <- xs]
              | otherwise = 0 : digitToInt s : [digitToInt x | x <- xs]

output :: BigNumber -> String
output (s:xs) | xs == [0] = "0"
            | s== 0  = '+' :  [intToDigit x | x <- xs]
            | otherwise = '-' :  [intToDigit x | x <- xs]

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

-- must be called such that x > y
somaAux :: Int -> BigNumber -> BigNumber -> BigNumber
somaAux 0 [] [] = []
somaAux carry [] [] = [carry]
somaAux carry (x : xs) [] = [(x + carry) `mod` 10] ++ somaAux ((x + carry) `div` 10) xs []
somaAux carry (x : xs) (y : ys) = [(x + y + carry) `mod` 10] ++ somaAux ((x + y + carry) `div` 10) xs ys

somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN x y
  | head x == head y = signal ++ zeroUntil (reverse (somaAux 0 (reverse bigger_list) (reverse smaller_list)))
  | otherwise = signal ++ zeroUntil (reverse (subAux 0 (reverse bigger_list) (reverse smaller_list)))
  where
    bigger_list
      | isBigger (drop 1 x) (drop 1 y) = drop 1 x --removing the signal here
      | otherwise = drop 1 y
    smaller_list
      | isBigger (drop 1 x) (drop 1 y) = drop 1 y --removing the signal here
      | otherwise = drop 1 x
    signal
      | isBigger (drop 1 x) (drop 1 y) = take 1 x
      | otherwise = take 1 y

subAux :: Int -> BigNumber -> BigNumber -> BigNumber
subAux carry [] [] = []
subAux carry (x : xs) []
  | x + carry < 0 = [(x + carry) + 10] ++ subAux (-1) xs []
  | otherwise = [(x + carry)] ++ subAux 0 xs []
subAux carry (x : xs) (y : ys)
  | x - y + carry < 0 = [(x - y + carry) + 10] ++ subAux (-1) xs ys
  | otherwise = [(x - y + carry)] ++ subAux 0 xs ys

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

--returns list with two BigNumbers [remainder, quociente]
bigNumberDiv :: BigNumber -> BigNumber -> BigNumber -> [BigNumber]
bigNumberDiv try x y
  | isBigger (drop 1 (mulBN y try)) (drop 1 x) = [subBN try [0, 1], subBN x (mulBN y (subBN try [0, 1]))]
  | otherwise = bigNumberDiv (somaBN try [0, 1]) x y

accumulateUntilBiggerOrEqual :: Int -> BigNumber -> BigNumber -> BigNumber
accumulateUntilBiggerOrEqual acc x y
  | isBigger (take acc x) y || isEqual (take acc x) y = take acc x
  | acc == length x = x
  | otherwise = accumulateUntilBiggerOrEqual (acc + 1) x y

divAux :: BigNumber -> BigNumber -> [Int] -> [[Int]]
divAux x y final
  | isBigger y x = [final, x]
  | otherwise = divAux (last partial_results ++ drop (length partial_quo) x) y (final ++ head partial_results)
  where
    partial_results = map (drop 1) (bigNumberDiv [1, 0] ([0] ++ partial_quo) ([0] ++ y)) --remove the signal on these operations
    partial_quo = accumulateUntilBiggerOrEqual 1 x y

divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)
divBN x y = (signal_quotient ++ head results_without_sign, signal_remainder ++ last results_without_sign)
  where
    results_without_sign = divAux x_no_signal y_no_signal []
    signal_quotient
      | head x == head y = [0]
      | otherwise = [1]
    signal_remainder = take 1 x
    x_no_signal = drop 1 x
    y_no_signal = drop 1 y

safeDivBN :: BigNumber -> BigNumber -> Maybe (BigNumber, BigNumber)
safeDivBN x y
  | isEqual y [0, 0] || isEqual y [1, 0] = Nothing
  | otherwise = Just (divBN x y)
