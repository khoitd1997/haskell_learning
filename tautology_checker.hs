data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Or Prop Prop | Equiv Prop Prop | Imply Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

type Assoc k v = [(k, v)]

type Subst = Assoc Char Bool

find :: Char -> Subst -> Bool
find k t = head [snd p | p <- t, fst p == k]

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Or p q) = eval s p || eval s q
eval _ (Equiv p q) = all (\sub -> eval sub p == eval sub q) (substs (And p q))
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : filter (/= x) (rmdups xs)

bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
  where
    bss = bools (n - 1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where
    vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = all (`eval` p) (substs p)