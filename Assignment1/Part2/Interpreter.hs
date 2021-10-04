module Interpreter where

import AbsNumbers

eval :: Exp -> Integer
eval (Num n) = n
eval (Plus n m) = (eval n) + (eval m)
eval (Times n m) = (eval n) * (eval m)
eval (Minus n m) = (eval n) - (eval m)
eval (Power n m) = (eval n) ^ (eval m)
eval (Divide n m) = div (eval n) (eval m)
--eval (Absolute n) = abs(eval n)
eval (Modulo n m) = (eval n) `mod` (eval m)
eval (Negate n) = (eval n) * (-1)
eval (Square n) = (eval n) ^ (2)
