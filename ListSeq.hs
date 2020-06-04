module ListSeq where

import Seq

(|||) :: a -> b -> (a, b)
a ||| b = (a, b)

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
mapear f (h: t) = h : t
             where
                (h,t) = (f h) ||| (mapear f t)
 
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
escanear :: (a -> a -> a) -> a -> [a] -> ([a], a)
escanear oplus n [] = ([],n)
escanear oplus n xs = (n : fst result, snd result)
    where result = escanear' oplus xs n

escanear' :: (a -> a -> a) -> [a] -> a -> ([a], a)
escanear' oplus [x] rs = ([], rs `oplus` x)
escanear' oplus (h: t) rs = 
    (e : (fst recursion), (snd recursion))
        where -- ver paralelizar
            e = rs `oplus` h
            recursion = escanear' oplus t e


{-
    Consideramos los costos sobre la longitud de la lista
    W (0) = c
    W (1) = c'
    W (n) = k' + W_escanear'(n) = k + W_escanear'(n-1)
    
    .°. W(n) = O(n)

    Consideramos los costos sobre la longitud de la lista
    D (0) = 1
    D (1) = 2
    D (n) = 1 + max(D_oplus +  D_escanear'(n))
    
    D_escanear'(n) = 1 + D_escanear'(n-1)
    .°. D_escanear'(n) = O(n)

    .°. D(n) = O(n)


    Análogo para la profundidad
-}
{-
s=  〈x0,           x1,         x2,                               x3〉
s′= (〈b,                      b⊕x0⊕x1,               〉,        b⊕x0⊕x1⊕x2⊕x3)
r= (〈b,           b⊕x0,      b⊕x0⊕x1,   b⊕x0⊕x1⊕x2〉,       b⊕x0⊕x1⊕x2⊕x3)


s=  〈x0,           x1,         x2,                 x3                                       x4〉
s′= (〈b,                       b⊕x0⊕x1                                             〉,    b⊕x0⊕x1⊕x2⊕x3⊕x4)
r= (〈b,        b⊕x0,          b⊕x0⊕x1,     b⊕x0⊕x1⊕x2         b⊕x0⊕x1⊕x2⊕x3〉,     b⊕x0⊕x1⊕x2⊕x3⊕x4)
-}

scanear oplus neutro s = (magia oplus s (fst s') , snd s')           -- r
    where s' = escanear oplus neutro (metamap oplus s)

magia oplus [] [] = []
magia oplus [x] [x'] = [x']
magia oplus (hs:_:ts) (hs':ts') = hs': (hs' `oplus` hs) : magia oplus ts ts'

f = \x y -> y+1



{-
    CUANDO TERMINEMOS LE BUSCAMOS EL COSTO
-}