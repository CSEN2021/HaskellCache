--General Part

data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)
data Tag = T Int deriving (Show, Eq)
data Data a = D a deriving (Show, Eq)

convertBinToDec :: Integral a => a -> a
convertBinToDec bin = convertBinToDecHelper bin 0
convertBinToDecHelper 0 _ = 0
convertBinToDecHelper bin p = ((mod bin 10)* (2 ^ p)) + convertBinToDecHelper (div bin 10) (p + 1)

replaceIthItem :: (Eq a, Num a) => t -> [t] -> a -> [t]
replaceIthItem item l 0 = (item:xs) where (x:xs) = l
replaceIthItem item l index = x:(replaceIthItem item xs (index-1)) where (x:xs) = l

splitEvery :: Int -> [a] -> [[a]]
splitEvery n l 
    | (length l) <= n = [l] 
    | otherwise = (a: (splitEvery n b)) where (a,b) = splitAt n l

logBase2 :: Floating a => a -> a
logBase2 num = logBase 2 num

getNumBits :: (Integral a, RealFloat a1) => a1 -> [Char] -> [c] -> a
getNumBits numOfSets cacheType cache = ceiling (logBase2 numOfSets)

fillZeros :: [Char] -> Int -> [Char]
fillZeros s 0 = s
fillZeros s n = ('0' : fillZeros s (n-1))

--Component 1

--Component 2

--Component 3

replaceInCache :: Integral b => Int -> Int -> [a] -> [Item a] -> [Char] -> b -> (a, [Item a])
replaceInCache tag idx memory oldCache "directMap" bitsNum = (retrievedData , newCache) 
    where 
        indexInMemoryBin = tag * (10^bitsNum) + idx
        retrievedData = memory !! convertBinToDec(indexInMemoryBin) 
        newCache = replaceIthItem ((It (T tag) (D retrievedData) True 0)) oldCache (convertBinToDec(idx))

--replaceInCache tag idx memory oldCache "fullyAssoc" bitsNum = ()
  --  where
