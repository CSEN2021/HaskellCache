--General Part
data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)
data Tag = T Int deriving (Show, Eq)
data Data a = D a deriving (Show, Eq)

convertBinToDec :: Integral a => a -> a
convertBinToDec bin = convertBinToDecHelper bin 0
convertBinToDecHelper 0 _ = 0
convertBinToDecHelper bin p = ((mod bin 10)* (2 ^ p)) + convertBinToDecHelper (div bin 10) (p + 1)

replaceIthItem :: (Eq a, Num a) => t -> [t] -> a -> [t]
replaceIthItem item (x:xs) 0 = (item:xs)
replaceIthItem item (x:xs) index = x:(replaceIthItem item xs (index-1))

splitEvery :: Int -> [a] -> [[a]]
splitEvery n (x:xs) 
    | (length (x:xs)) <= n = [(x:xs)]
    | otherwise = (a: (splitEvery n b)) where (a,b) = splitAt n (x:xs)

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