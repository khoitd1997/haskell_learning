import Data.Char

myMult :: Float -> (Float -> (Float -> Float))
myMult x y z = x * y * z

luhnDouble :: Int -> Int
luhnDouble x
  | doubledNumber > 9 = doubledNumber - 9
  | otherwise = doubledNumber
  where
    doubledNumber = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d =
  let total = luhnDouble a + luhnDouble b + luhnDouble c + luhnDouble d
   in total `mod` 10 == 0

myLambdaMult :: Float -> (Float -> (Float -> Float))
myLambdaMult = \x y z -> x * y * z

myRecip :: Fractional a => a -> a
myRecip n = 1 / n

myHead (x : _) = x

mySignum :: (Ord a1, Num a1, Num a2) => a1 -> a2
mySignum n
  | n > 0 = -2
  | n < 0 = -3
  | otherwise = -4

myPatternMatchFunc :: Int -> Int -> Int
myPatternMatchFunc 5 4 = 5
myPatternMatchFunc _ 4 = 7
myPatternMatchFunc _ _ = 8

myFactor :: Int -> [Int]
myFactor n = [x | x <- [1 .. n], n `mod` x == 0]

myConst :: a -> (b -> a)
myConst x = \_ -> x

lambdaResult = (* 2) 2

myPositions :: Ord x => x -> [x] -> [Int]
myPositions x y = [i | (x', i) <- zip y [0 ..], x' == x]

myThird :: [a] -> a
myThird (_ : _ : x : _) = x

mySafeTail :: [a] -> [a]
mySafeTail [] = []
mySafeTail x = tail x

mySum100 = sum [x ^ 2 | x <- [1.100]]

myGrid :: Int -> Int -> [(Int, Int)]
myGrid x y = [(x', y') | x' <- [0 .. x], y' <- [0 .. y]]

mySquare :: Int -> [(Int, Int)]
mySquare n = [(x', y') | x' <- [0 .. n], y' <- [0 .. n], x' /= y']

myReplicate :: Int -> a -> [a]
myReplicate count elem = [elem | _ <- [1 .. count]]

main :: IO ()
-- main = putStrLn (show (res))
--   where
--     res = b + c
--     b = 1
--     c = 9

myPyths :: Int -> [(Int, Int, Int)]
myPyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x ^ 2 + y ^ 2 == z ^ 2]

myPerfectFactor :: Int -> [Int]
myPerfectFactor n = [x | x <- [1 .. n], n `mod` x == 0, x /= n]

myPerfect :: Int -> [Int]
myPerfect n = [x | x <- [1 .. n], sum (myPerfectFactor x) == x]

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

mySumDown :: Int -> Int
mySumDown 0 = 0
mySumDown x = x + mySumDown (x - 1)

myPower :: Int -> Int -> Int
myPower x 1 = x
myPower x y = x * myPower x (y - 1)

myEuclid :: Int -> Int -> Int
myEuclid x y
  | x == y = x
  | x > y = myEuclid (x - y) y
  | otherwise = myEuclid x (y - x)

myMerge :: Ord a => [a] -> [a] -> [a]
myMerge [] b = b
myMerge a [] = a
myMerge x y =
  if (head x) < (head y)
    then (head x) : myMerge (tail x) y
    else (head y) : myMerge x (tail y)

myHalves :: [a] -> ([a], [a])
myHalves x = splitAt (length x `div` 2) x

myTwice :: (a -> a) -> a -> a
myTwice f x = f (f x)

mySum :: Num a => [a] -> a
mySum = foldr (+) 0

myMergeSort :: Ord a => [a] -> [a]
myMergeSort [] = []
myMergeSort [x] = [x]
myMergeSort l = myMerge (myMergeSort (fst halves)) (myMergeSort (snd halves))
  where
    halves = myHalves l

type MyBit = Int

bin2int :: [MyBit] -> Int
bin2int bits = sum [w * b | (w, b) <- zip weights bits]
  where
    weights = iterate (* 2) 1

myChop8 :: [MyBit] -> [[MyBit]]
myChop8 [] = []
myChop8 xs = take 8 xs : myChop8 (drop 8 xs)

myAll :: (a -> Bool) -> [a] -> Bool
myAll pred = foldr (\x y -> pred x && y) True

myAny :: (a -> Bool) -> [a] -> Bool
myAny pred = foldr (\x y -> pred x || y) True

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile pred (x : xs) =
  if pred x
    then x : myTakeWhile pred xs
    else []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ [] = []
myDropWhile pred (x : xs) =
  if pred x
    then myDropWhile pred xs
    else x : xs

myMapF :: (a -> b) -> [a] -> [b]
myMapF f = foldr (\a b -> (f a) : b) []

myFilterP :: (a -> Bool) -> [a] -> [a]
myFilterP f = foldr (\a b -> if f a then a : b else b) []

myDec2Int :: [Int] -> String
myDec2Int = foldl (\a b -> a ++ show b) ""

myUnfold p h t x
  | p x = []
  | otherwise = h x : myUnfold p h t (t x)

unfoldChop8 :: [MyBit] -> [[MyBit]]
unfoldChop8 = myUnfold null (take 8) (drop 8)

unfoldMapF :: (a -> b) -> [a] -> [b]
unfoldMapF f = myUnfold null (f . (!! 0)) (drop 1)

unfoldIterateF :: (a -> a) -> a -> [a]
unfoldIterateF = myUnfold (const False) id

myAltMap :: (a -> b) -> (a -> b) -> [a] -> [b]
myAltMap f1 f2 [] = []
myAltMap f1 f2 [x] = [f1 x]
myAltMap f1 f2 (x : y : xs) = [f1 x, f2 y] ++ myAltMap f1 f2 xs

myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f x y = f (x, y)

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (a, b) = f a b

data MyBool = MyTrue | MyFalse

data MyShape = MyCircle | MyRect Float Float

myArea :: MyShape -> Float
myArea (MyCircle) = pi
myArea (MyRect x y) = x * y

main = putStrLn (show (res))
  where
    res = myArea (MyCircle)