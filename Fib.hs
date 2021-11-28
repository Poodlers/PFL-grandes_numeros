import BigNumber (BigNumber,scanner, output,somaBN, subBN,mulBN,divBN)
import Utils (enesimo,generateList)
-- recursive approach
fibRec :: (Integral a) => a -> a
fibRec n | n<2 = n -- case bases
        | otherwise = fibRec (n-2) + fibRec (n-1) 

fibLista :: (Integral a) => a -> a
fibLista n = last fibo  
        where fibo = 0 : 1 : [fibo!! fromIntegral (i-2) + fibo !! fromIntegral (i-1) | i<-[2..n]] -- generates list with every fibonacci number from index 0 to n


fibListaInfinita :: (Integral a) => a -> a
fibListaInfinita n = fibs !! fromIntegral n 
        where fibs = 0 : 1 : zipWith (+) fibs (tail fibs) -- generates infinite list with every fibonacci number 

--recursive approach with big numbers 
fibRecBN :: BigNumber-> BigNumber
fibRecBN [0, 0] = [0 , 0] 
fibRecBN [1, 0] = [0 , 0] --two ways of representing 0 
fibRecBN [0, 1]= [0, 1]
fibRecBN n = somaBN (fibRecBN ( subBN n [0,2])) (fibRecBN (subBN n [0,1]))

fibListaBN :: BigNumber -> BigNumber
fibListaBN n = enesimo fibo n  
        where fibo = [0, 0] : [0, 1] : [ somaBN (enesimo fibo (subBN i [0, 1]))  (enesimo fibo (subBN i [0, 2])) | i<- (generateList [0,2] n)] -- generates list with every fibonacci number in big number from index 0 to n


fibListaInfinitaBN :: BigNumber -> BigNumber
fibListaInfinitaBN n = enesimo fibs n 
        where fibs = [0,0] : [0,1] : zipWith somaBN fibs (tail fibs)-- generates infinite list with every fibonacci number in big numbers

