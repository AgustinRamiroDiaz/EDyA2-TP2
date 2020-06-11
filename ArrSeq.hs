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


filtrar :: (a -> Bool) -> A.Arr a -> A.Arr a
filtrar p ap = joinS $ tabulateS (\i-> let elem = (ap ! i) 
                                         in 
                                           if p elem then singletonS elem
                                                     else emptyS
                                 ) (lengthS ap)


concatenar :: A.Arr a -> A.Arr a -> A.Arr a
concatenar a b  | l1 == 0 = b
                | l2 == 0 = a
                | otherwise = tabulateS (\i-> if i < l1 then a ! i else b ! (i - l1) ) lt 
                where
                    (l1,l2) = (lengthS a) ||| (lengthS b) 
                    lt = (l1 + l2)

mostrarArbol :: A.Arr a -> TreeView a (A.Arr a)
mostrarArbol arr | lAP == 1  = ELT (nthS arr 0)
                 | lAP  > 1  = NODE (takeS arr m) (dropS arr m)
                 | otherwise = EMPTY
                 where
                   lAP = lengthS arr
                   m   = div lAP 2 

mostrarLista :: A.Arr a -> ListView a (A.Arr a)
mostrarLista arr | lAP == 0   = NIL
                 | otherwise  = CONS (nthS arr 0) (dropS arr 1)
                 where
                   lAP = lengthS arr 


reducir :: (a -> a -> a) -> a -> A.Arr a -> a
reducir oplus neutro ap  
  | lAP == 0 = neutro
  | lAP == 1 = oplus neutro (nthS ap 0)
  | otherwise = reducir oplus neutro (contraer oplus ap lAP)
    where
    lAP = lengthS ap


escanear :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
escanear oplus neutro ap
  | lAP == 0  = (emptyS, neutro)
  | lAP == 1  = (singletonS neutro, neutro `oplus` (ap ! 0))
  | otherwise = (expandir oplus neutro ap (fst s') lAP, snd s')
    where
      lAP = lengthS ap 
      s' = escanear oplus neutro (contraer oplus ap lAP)

--Funciones auxiliares
contraer :: (a -> a -> a) -> A.Arr a -> Int -> A.Arr a
contraer oplus ap lAP | mod lAP 2 == 0 = tabulateS (\x -> f x) mitad
                      | otherwise      = tabulateS (\x -> if x /= mitad then f x else ap ! (lAP - 1)) (mitad + 1)
                      where
                        mitad = div lAP 2
                        f x = oplus (ap ! (2 * x)) (ap ! (2 * x + 1))

expandir :: (a -> a -> a) -> a -> A.Arr a -> A.Arr a-> Int -> A.Arr a
expandir oplus neutro s s' ls = tabulateS (\x -> if even x then s' ! (div x 2) else oplus (s' ! (div x 2)) (s ! (x - 1))) ls
