data Tree a = Leaf a | Node (Tree a) (Tree a)

countTotalLeaves :: Tree a -> Int
countTotalLeaves (Leaf _) = 1
countTotalLeaves (Node l r) = countTotalLeaves l + countTotalLeaves r

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) = abs (countTotalLeaves l - countTotalLeaves r) <= 1 && balanced l && balanced r

splitBalanceList :: [a] -> ([a], [a])
splitBalanceList xs = splitAt half xs
  where
    half = length xs `div` 2

balance :: [a] -> Tree a
balance [a] = Leaf a
balance xs = Node (balance (fst split)) (balance (snd split))
  where
    split = splitBalanceList xs

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val v) = f v
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)

eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

data MyMaybe a = MyNothing | MyJust a

instance Eq a => Eq (MyMaybe a) where
  MyNothing == MyNothing = True
  MyJust x == MyJust y = x == y
  _ == _ = False

main = putStrLn (show (res))
  where
    -- res = eval (Add (Val 5) (Val 6))
    -- res = eval (Add (Add (Val 4) (Val 5)) (Val 7))
    res = size (Add (Add (Val 4) (Val 5)) (Val 7))