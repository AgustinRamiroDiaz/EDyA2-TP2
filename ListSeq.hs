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
                (h, t) = (f i) ||| (tabular' f (n-1) (i+1)) 

mapear :: (a -> b) -> [a] -> [b]
mapear f [] = []
mapear f (h: t) = h' : t'
             where
                (h',t') = (f h) ||| (mapear f t)


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

reducir :: (a -> a -> a) -> a -> [a] -> a
reducir oplus neutro [] = neutro
reducir oplus neutro [x] = oplus neutro x
reducir oplus neutro sequencia = reducir oplus neutro (contraer oplus sequencia)

escanear :: (a -> a -> a) -> a -> [a] -> ([a], a)
escanear oplus neutro [] = ([], neutro)
escanear oplus neutro [x] = ([neutro], neutro `oplus` x)
escanear oplus neutro s = (expandir oplus s (fst s') , snd s')
    where s' = escanear oplus neutro (contraer oplus s)

--Funciones auxiliares
contraer :: (a -> a -> a) -> [a] -> [a]
contraer oplus [] = []
contraer oplus [x] = [x]
contraer oplus (x:y:tail) = h:t  
                where (h,t) = (oplus x y) ||| (contraer oplus tail)

expandir :: (a -> a -> a) -> [a] -> [a] -> [a]
expandir oplus [] [] = []
expandir oplus [x] [x'] = [x']
expandir oplus (hs:_:ts) (hs':ts') = hs': h : t
                                  where
                                      (h,t) = (hs' `oplus` hs) ||| (expandir oplus ts ts')
