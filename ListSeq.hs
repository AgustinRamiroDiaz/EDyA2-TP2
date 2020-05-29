module ListSeq where

import Seq

data TreeView a = E | EL a | Node [a] [a]

instance Seq [] where
    emptyS = vacia
    singletonS = singular
    lengthS = longitud
    nthS = enesimo
    tabulateS = tabular
    mapS = mapear
    filterS = filtrar
    appendS = concatenarLista
    takeS lista n = take n lista
    dropS lista n = drop n lista
    showtS        = mostrarArbol    
    showlS = mostrarLista
    joinS  = aplanar
    -- reduceS    :: (a -> a -> a) -> a -> s a -> a
    scanS = escanear
    fromList = desdeLista

vacia :: [a]
vacia = []

singular :: a -> [a]
singular x = [x]

longitud :: [a] -> Int
longitud [] = 0
longitud (h: t) = 1 + longitud t

enesimo :: [a] -> Int -> a
enesimo (h: t) 0 = h
enesimo (h: t) n = enesimo t (n-1)

tabular :: (Int -> a) -> Int -> [a]
tabular f 0 = []
tabular f n = tabular' f n 0

tabular' :: (Int -> a) -> Int -> Int -> [a]
tabular' f 1 i = [f i] 
tabular' f n i = f i : (tabular' f (n-1) (i+1)) 

mapear :: (a -> b) -> [a] -> [b]
mapear f [] = []
mapear f (h: t) = (f h): (mapear f t)

filtrar :: (a -> Bool) -> [a] -> [a] 
filtrar p [] = []
filtrar p (h: t)
    | p h = h: (filtrar p t)
    | otherwise = filtrar p t


concatenarLista :: [a] -> [a] -> [a]
concatenarLista xs ys = xs ++ ys


mostrarArbol :: [a] -> TreeView a [a]
mostrarArbol []     = E
mostrarArbol [x]    = EL x
mostrarArbol (x:xs) = Node (take seq  (l / 2)) Node (drop seq  (l / 2))
                      where
                           l = longitud (x:xs)


aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar [[x]] = [x]
aplanar (xs : xss) = appendS xs (aplanar xss)


mostrarLista :: [a] -> ListView a [a]
mostrarLista [] = NIL
mostrarLista (h: t) = CONS h t











escanear :: (a -> a -> a) -> a -> [a] -> ([a], a)
escanear oplus n xs = escanear' oplus xs n

escanear' :: (a -> a -> a) -> [a] -> a -> ([a], a)
escanear' oplus [x] rs = ([], rs `oplus` x)
escanear' oplus (h: t) rs = 
    (e : (fst recursion), (snd recursion))
        where
            e = rs `oplus` h
            recursion = escanear' oplus t e






desdeLista :: [a] -> [a]
desdeLista xs = xs