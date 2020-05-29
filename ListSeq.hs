module ListSeq where

import Seq
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
tabular' f n i = f i : (tabular' f (n-1) (i+1)) 

mapear :: (a -> b) -> [a] -> [b]
mapear f [] = []
mapear f (h: t) = (f h): (mapear f t)

concatenarLista :: [a] -> [a] -> [a]
concatenarLista [] ys = ys
concatenarLista (h: t) ys = h : (concatenarLista t ys)

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

aArbol :: (a -> a -> a)-> [a] -> a
aArbol oplus [x] = x 
aArbol oplus sequencia = oplus (aArbol oplus left) (aArbol oplus right)
    where 
        n = length sequencia
        m = 2 ^ (floor $ (logBase 2 (fromIntegral (n - 1))))
        (left, right) = splitAt m sequencia

reducir :: (a -> a -> a) -> a -> [a] -> a
reducir oplus neutro [] = neutro
reducir oplus neutro sequencia = oplus neutro (aArbol oplus sequencia)

escanear :: (a -> a -> a) -> a -> [a] -> ([a], a)
escanear oplus n xs = escanear' oplus xs n

escanear' :: (a -> a -> a) -> [a] -> a -> ([a], a)
escanear' oplus [x] rs = ([], rs `oplus` x)
escanear' oplus (h: t) rs = 
    (e : (fst recursion), (snd recursion))
        where
            e = rs `oplus` h
            recursion = escanear' oplus t e
