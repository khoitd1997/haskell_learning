import Control.Exception (assert)
import Data.List (sort, sortBy)

data Op = Add | Sub | Mul | Div

type Result = (Expr, Int)

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n]
eval (App o l r) =
  [ apply o x y | x <- eval l, y <- eval r, valid o x y
  ]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = yss ++ map (x :) yss
  where
    yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
-- concat (map (interleave x) (perms xs))
perms (x : xs) = [perm | xp <- perms xs, perm <- interleave x xp]

choices :: [a] -> [[a]]
choices xs = [choice | sub <- subs xs, choice <- perms sub]

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns =
  [ e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r
  ]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns, lx <- results ls, ry <- results rs, res <- combine' lx ry]

matchSort :: Int -> Int -> Int -> Ordering
matchSort n m1 m2
  | diff < 0 = LT
  | diff > 0 = GT
  | otherwise = EQ
  where
    diff = abs (n - m1) - abs (n - m2)

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = map fst (if null exactMatches then closestMatches else exactMatches)
  where
    s = [r | ns' <- choices ns, r <- results ns']
    exactMatches = filter (\(e, m) -> m == n) s
    closestMatches = take 5 (sortBy (\(e1, m1) (e2, m2) -> matchSort n m1 m2) s)

main :: IO ()
main =
  -- assert (length (solutions' [1, 3, 7, 10, 25, 50] 765) == 49) print "success!"
  print (solutions' [1, 3, 7, 10, 25, 50] 831)
