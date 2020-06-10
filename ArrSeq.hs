module ArrSeq where

import Seq

import qualified Arr as A

import  Arr ((!))

import Par ((|||))


instance Seq A.Arr where
    emptyS = A.empty
    singletonS x = A.tabulate (\_-> x) 1
    lengthS = A.length
    nthS seq n =  seq ! n
    tabulateS = A.tabulate
    mapS = mapear
    filterS = filtrar
    appendS = concatenar 
    takeS seq n = A.subArray 0 n seq
    dropS seq n = A.subArray n ((lengthS seq)  - n) seq
    showtS = mostrarArbol
    showlS = mostrarLista
    joinS = A.flatten
    reduceS = reducir
    scanS = escanear
    fromList = A.fromList


mapear :: (a -> b) -> A.Arr a -> A.Arr b
mapear f ap = tabulateS (\i -> f (ap ! i)) (lengthS ap) 


mostrarArbol :: A.Arr a -> TreeView a (A.Arr a)
mostrarArbol arr | lAP == 1  = ELT (nthS arr 0)
                 | lAP  > 1  = NODE (takeS arr m) (dropS arr (lAP - m))
                 | otherwise = EMPTY
                 where
                   lAP = lengthS arr
                   m   = div lAP 2 

mostrarLista :: A.Arr a -> ListView a (A.Arr a)
mostrarLista arr | lAP == 0   = NIL
                 | otherwise  = CONS (nthS arr 0) (dropS arr 1)
                 where
                   lAP = lengthS arr 

concatenar :: A.Arr a -> A.Arr a -> A.Arr a
concatenar a b  | l1 == 0 = b
                | l2 == 0 = a
                | otherwise = tabulateS (\i-> if i < l1 then a ! i else b ! (i - l1) ) lt 
                where
                    (l1,l2) = (lengthS a) ||| (lengthS b) 
                    lt = (l1 + l2)

filtrar :: (a -> Bool) -> A.Arr a -> A.Arr a
filtrar p ap = joinS $ tabulateS (\i-> let elem = (ap ! i) 
                                         in 
                                           if p elem then singletonS elem
                                                     else emptyS
                                 ) (lengthS ap)

reducir :: (a -> a -> a) -> a -> A.Arr a -> a
reducir oplus neutro ap  | lAP == 0  = neutro
                         | otherwise = oplus neutro (reducir' oplus ap)
                         where 
                         lAP = lengthS ap

reducir' :: (a -> a -> a)-> A.Arr a -> a
reducir' oplus ap | n == 1  = ap ! 0  
                  | otherwise = oplus left' right'
    where 
        n = lengthS ap
        m = 2 ^ (floor $ (logBase 2 (fromIntegral (n - 1))))
        (left, right) = (takeS ap m) ||| (dropS ap m)
        (left', right') = (reducir' oplus left) ||| (reducir' oplus right)


escanear :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
escanear oplus neutro ap
  | lAP == 0  = (emptyS, neutro)
  | lAP == 1  = (singletonS neutro, neutro `oplus` (ap ! 0))
  | otherwise = (expandir oplus neutro ap (fst s'), snd s')
    where
      lAP = lengthS ap 
      s' = escanear oplus neutro (contraer oplus ap)

contraer :: (a -> a -> a) -> A.Arr a -> A.Arr a
contraer oplus ap | mod lAP 2 == 0 = tabulateS (\ x ->  oplus (ap ! (2 * x)) (ap ! (2 * x + 1))) mitad
                 | otherwise      = tabulateS (\ x ->  if x /= mitad then oplus (ap ! (2 * x)) (ap ! (2 * x + 1)) else ap ! (lAP - 1)) techoMitad
                 where
                    lAP   = lengthS ap
                    mitad = div lAP 2
                    techoMitad = mitad + 1 

expandir :: (a -> a -> a) -> a -> A.Arr a -> A.Arr a-> A.Arr a
expandir oplus neutro s s' = tabulateS (\x -> if (mod x 2) == 0 then s' ! (div x  2) else oplus (s' ! (div x 2)) (s ! (x - 1))) ls
                        where
                            ls = lengthS s
