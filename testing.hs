import ListSeq
import ArrSeq
import Seq
import qualified Arr as A


jota = \x y -> y+1

seqToList :: Seq s => s a -> [a]
seqToList v
    | n == 0 = []
    | otherwise = nthS v 0 : seqToList (dropS v 1)
        where n = lengthS v


testScan fun neutro seq = scanSeq1 == (seqToList (fst scanSeq2), snd scanSeq2)
    where 
        scanSeq1 = scanS fun neutro seq
        scanSeq2 = scanS fun neutro (fromList seq :: A.Arr Integer)



testReduce fun neutro seq = reduceSeq1 == reduceSeq2
    where 
        reduceSeq1 = reduceS fun neutro seq
        reduceSeq2 = reduceS fun neutro (fromList seq :: A.Arr Integer)