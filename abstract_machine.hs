data Expr = Val Int | Add Expr Expr | Mul Expr Expr

data Op = EVAL Expr | ADD Int | MUL Int

type Cont = [Op]

value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y
value (Mul x y) = value x * value y

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)
eval (Mul x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (n + m)
exec (MUL n : c) m = exec c (n * m)
