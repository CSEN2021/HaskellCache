--General Part
data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)
data Tag = T Int deriving (Show, Eq)
data Data a = D a deriving (Show, Eq)

convertBinToDec :: Integral a => a -> a

convertBinToDecHelper 0 _ = 0
convertBinToDecHelper bin p = ((mod bin 10)* (2 ^ p)) + convertBinToDecHelper (div bin 10) (p + 1)
convertBinToDec bin = convertBinToDecHelper bin 0

replaceIthItem :: (Eq a, Num a) => t -> [t] -> a -> [t]
replaceIthItem a (x:xs) 0 = (a:xs)
replaceIthItem a (x:xs) i = x:(replaceIthItem a xs (i-1))

{-splitEvery :: Int -> [a] -> [[a]]
splitEvery n l = [splitEveryHelper n n l]
splitEveryHelper _ _ [] = []
splitEveryHelper n 0 (x:xs) = (x:(splitEveryHelper n n xs))
splitEveryHelper n c (x:xs) = [x] ++ splitEveryHelper n (c-1) xs-}

splitEvery n l = if 
splitEvery n (x:xs) = (a:splitEvery n xs) where (a,b) = splitAt n (x:xs)

logBase2 :: Floating a => a -> a
logBase2 num = logBase 2 num


--Component 1

--Component 2

--Component 3