module ListSeq where

import Seq

import Par ((|||))

--mapS , appendS, reduceS, scanS {Hacer nosotros}
instance Seq [] where
    emptyS = []
    singletonS x = [x]
    lengthS = length
    nthS l n = l !! n
    tabulateS = tabular
    mapS = mapear
    filterS = filter
    appendS = concatenarLista
    takeS lista n = take n lista
    dropS lista n = drop n lista
    showtS = mostrarArbol
    showlS = mostrarLista
    joinS = concat
    reduceS = reducir
    scanS = escanear
    fromList = id

tabular :: (Int -> a) -> Int -> [a]
tabular f 0 = []
tabular f n = tabular' f n 0

tabular' :: (Int -> a) -> Int -> Int -> [a]
tabular' f 1 i = [f i] 
tabular' f n i = h : t
             where
                (h,t) = (f i) ||| (tabular' f (n-1) (i+1)) 

mapear :: (a -> b) -> [a] -> [b]
mapear f [] = []
mapear f (h: t) = h' : t'
             where
                (h',t') = (f h) ||| (mapear f t)
 
{-
    Consideramos los costos sobre la longitud de la lista
    W (0) = c
    W (n) = k' + W (n-1) + W_f(n) <= W(n-1) + k
    .°. W(n) = O(n)

    ---VER--- Análogo para la profundidad
-}

concatenarLista :: [a] -> [a] -> [a]
concatenarLista [] ys = ys
concatenarLista (h: t) ys = h : (concatenarLista t ys)

{-
    Consideramos los costos sobre la longitud de la primer lista
    W (0) = c
    W (n) = k + W (n-1)
    .°. W(n) = O(n)

    Análogo para la profundidad
-}


mostrarArbol :: [a] -> TreeView a [a]
mostrarArbol []     = EMPTY
mostrarArbol [x]    = ELT x
mostrarArbol xs     = NODE  left  right
                        where
                            mitad = div (length xs) 2
                            (left, right) = splitAt mitad xs

mostrarLista :: [a] -> ListView a [a]
mostrarLista [] = NIL
mostrarLista (h: t) = CONS h t

longitud [] = 0
longitud (h:t) = 1 + longitud t


reducir :: (a -> a -> a) -> a -> [a] -> a
reducir oplus neutro [] = neutro
reducir oplus neutro [x] = oplus neutro x
reducir oplus neutro sequencia = reducir oplus neutro (metamap oplus sequencia)

metamap :: (a -> a -> a) -> [a] -> [a]
metamap oplus [] = []
metamap oplus [x] = [x]
metamap oplus (x:y:tail) = h:t  
                where (h,t) = (oplus x y) ||| (metamap oplus tail)

{-
  COSTO METAMAP

    W (0) = c
    W (1) = c'
    W (n) = W oplus + W (n-2) + k = ... O(n)

    S (0) = c
    s (1) = c'
    s (n) = MAX ( S_oplus , S (n-2) ) + k = ... O(n)


 COSTO REDUCIR

    W (0) = c
    W (1) = W_oplus = c'
    W (n) = W_metamap (n) + W_oplus + W (n/2) + k <= k*n + W(n/2)   .*. W (n) = O(n), por tercer
    caso del teorema maestro y suavidad

 ANALOGO PROFUNDIDAD

-}
---------------------------------------------------------------

escanear oplus neutro [] = ([], neutro)
escanear oplus neutro [x] = ([neutro], neutro `oplus` x)
escanear oplus neutro s = (completar oplus s (fst s') , snd s')           -- r
    where s' = escanear oplus neutro (metamap oplus s)

completar oplus [] [] = []
completar oplus [x] [x'] = [x']
completar oplus (hs:_:ts) (hs':ts') = hs': (hs' `oplus` hs) : completar oplus ts ts'

f = \x y -> y+1

{-
    Costo de completar
    W(completar oplus 0) = c0
    W(completar oplus 1) = c1
    W(completar oplus n) = W(completar oplus n-2) + W(oplus) + c => O(n)

    S(completar oplus 0) = c0
    S(completar oplus 1) = c1
    S(completar oplus n) = S(completar oplus n-2) + c => O(n)
-}

{-
    Costo escanear
    W(escanear oplus 0) = c0
    W(escanear oplus 0) = c1 + W(oplus) = c2
    W(escanear oplus n) = W(metamap oplus n) + W(escanear oplus n/2) + W(oplus) + W(completar oplus n) c = W(escanear oplus n/2) + c'n => O(n^2)

    D(escanear oplus 0) = c0
    D(escanear oplus 0) = c1 + D(oplus) = c2
    D(escanear oplus n) = D(metamap oplus n) + D(escanear oplus n/2) + D(completar oplus n) + c = D(escanear oplus n/2) + c'n => O(n^2)
-}