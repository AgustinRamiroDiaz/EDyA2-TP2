module ArrSeq where

import Seq

import qualified Arr as A

import  Arr ((!))


instance Seq A.Arr where

    emptyS = A.empty
    singletonS x = A.tabulate (\_-> x) 1
    lengthS = A.length
    nthS seq n =  (!) seq n
    tabulateS = A.tabulate
    --mapS = mapear
    --filterS = filter
    --appendS seq1 seq2 =    FLATTEN ARRAY SEQ 1 SEQ 2
    takeS seq n = A.subArray 0 n seq
    dropS seq n = A.subArray n (lengthS seq) seq
    showtS = mostrarArbol
    showlS = mostrarLista
    --joinS = concat
    --reduceS = reducir
    --scanS = escanear
    --fromList = id

{-
  mapear :: (a -> b) -> AP a -> AP b
  mapear f emptyS = emptyS
  mapear f ap | lAP == 1  = singletonS (f (ap `!` 0) )  
              | lAP  > 1  = appendS (f (ap `!` 0) ) (mapear f (dropS ap 1))
              where
                   lAP = lengthS ap
-}

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

