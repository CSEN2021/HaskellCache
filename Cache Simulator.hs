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

--Component 1 & 2

--convertAddress:: (Integral b1, Integral b2) => b1 -> b2 -> p -> (b1, b1) Breaks code
convertAddress binAddress bitsNum "directMap" = ((div binAddress (10 ^ bitsNum)), (mod binAddress (10 ^ bitsNum)))
convertAddress binAddress bitsNum "setAssoc"  = ((div binAddress (10 ^ bitsNum)), (mod binAddress (10 ^ bitsNum)))

searchSet _ [] _ = NoOutput
searchSet t ((It (T tag) (D d) validBit _):xs) acc 
    | ((validBit == True) && (tag == t)) = Out (d,acc)
    | otherwise = searchSet t xs (acc + 1)

getDataFromCache :: (Integral b, Eq a) => [Char] -> [Item a] -> [Char] -> b -> Output a
data Output a = Out (a, Int) | NoOutput deriving (Show, Eq)
getDataFromCache stringAddress cache "directMap" bitsNum
    | ((tag == fst(convertAddress (read stringAddress :: Int) bitsNum "directMap")) && (validBit == True)) = Out (d,0)
    | otherwise = NoOutput
        where 
            indx = convertBinToDec (snd(convertAddress (read stringAddress :: Int) bitsNum "directMap"))
            (It (T tag) (D d) validBit order) = cache !! indx

getDataFromCache stringAddress cache "setAssoc" bitsNum = searchSet tag list 0
    where 
        tag = fst(convertAddress (read stringAddress :: Int) bitsNum "setAssoc")
        ind = snd(convertAddress (read stringAddress :: Int) bitsNum "setAssoc")
        list = (splitEvery ((div (length cache) 2^bitsNum)) cache) !! ind

--Component 3

searchCache [] _ oldestIndex _ = oldestIndex
searchCache ((It (T _) (D _) validBit order):xs) index oldestIndex oldestOrder
    | validBit == False = index
    | oldestOrder < order = (searchCache xs (index + 1) index order)
    | otherwise = (searchCache xs (index + 1) oldestIndex oldestOrder)

incrementCache [] = []
incrementCache ((It (T tag) (D d) validBit order):xs)
    | validBit = (It (T tag) (D d) validBit (order+1)):incrementCache(xs)
    | otherwise = (It (T tag) (D d) validBit order):incrementCache(xs)
    
replaceInCache :: Integral b => Int -> Int -> [a] -> [Item a] -> [Char] -> b -> (a, [Item a])
replaceInCache tag idx memory oldCache "directMap" bitsNum = (retrievedData , newCache) 
    where 
        indexInMemoryBin = tag * (10 ^ bitsNum) + idx
        retrievedData = memory !! convertBinToDec(indexInMemoryBin) 
        newCache = replaceIthItem ((It (T tag) (D retrievedData) True 0)) oldCache (convertBinToDec(idx))

replaceInCache tag idx memory oldCache "fullyAssoc" bitsNum = (retrievedData , newCache)
    where
        retrievedData = memory !! convertBinToDec(tag)
        indexToPutAt = searchCache oldCache 0 (-1) (-1)
        midCache = incrementCache oldCache
        newCache = replaceIthItem ((It (T tag) (D retrievedData) True 0)) midCache indexToPutAt
        
replaceInCache tag idx memory oldCache "setAssoc" bitsNum = (retrievedData , newCache)
    where
        indexInMemoryBin = tag * (10 ^ bitsNum) + idx
        retrievedData = memory !! convertBinToDec(indexInMemoryBin)
        setSize = div (length oldCache) (2 ^ bitsNum)
        listOfSets = splitEvery setSize oldCache
        theSet = listOfSets !! convertBinToDec(idx)
        indxInTheSet = searchCache theSet 0 (-1) (-1)
        newSet = incrementCache theSet
        newSetWithItm = replaceIthItem ((It (T tag) (D retrievedData) True 0)) newSet indxInTheSet
        newListOfSets = replaceIthItem newSetWithItm listOfSets (convertBinToDec idx)
        newCache = foldr (++) [] newListOfSets

--Pre - Implemented Fucntions
getData stringAddress cache memory cacheType bitsNum
    | x == NoOutput = replaceInCache tag index memory cache cacheType bitsNum
    | otherwise = (getX x, cache)
    where
        x = getDataFromCache stringAddress cache cacheType bitsNum
        address = read stringAddress:: Int
        (tag, index) = convertAddress address bitsNum cacheType
        getX (Out (d, _)) = d

runProgram [] cache _ _ _ = ([], cache)
runProgram (addr: xs) cache memory cacheType numSets = ((d:prevData), finalCache)
    where
        bitsNum = round(logBase2 numSets)
        (d, updatedCache) = getData addr cache memory cacheType bitsNum
        (prevData, finalCache) = runProgram xs updatedCache memory cacheType numSets