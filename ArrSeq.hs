module ArrSeq where

import Seq

import qualified Arr as A

import  Arr (!)


instance Seq AP where

    emptyS = A.empty
    singletonS x = A (A.V.singleton x)
    lengthS = length
    nthS l n = l `!` n
    tabulateS = tabulate
    --mapS = mapear
    filterS = filter
    --appendS = concatenarLista
    takeS lista n = take n lista
    dropS lista n = drop n lista
    showtS = mostrarArbol
    showlS = mostrarLista
    joinS = concat
    --reduceS = reducir
    --scanS = escanear
    fromList = id

{-
  mapear :: (a -> b) -> AP a -> AP b
  mapear f emptyS = emptyS
  mapear f ap | lAP == 1  = singletonS (f (ap `!` 0) )  
              | lAP  > 1  = appendS (f (ap `!` 0) ) (mapear f (dropS ap 1))
              where
                   lAP = lengthS ap
-}