module Calc.Op
    ( neg
    , pow
    , mul
    , iDiv
    , add
    , sub
    , bind
    ) where

import Calc.SyntaxTree (Expr(..))

neg (Val i) = Val $ -i
neg e1      = Negative e1

pow (Val i) (Val j) = Val $ i ^ j
pow e1 e2           = Pow e1 e2

mul (Val i) (Val j) = Val $ i * j
mul e1 e2           = Mul e1 e2

iDiv (Val i) (Val j) = Val $ i `div` j
iDiv e1 e2           = Div e1 e2

add (Val i) (Val j) = Val $ i + j
add e1 e2           = Add e1 e2

sub (Val i) (Val j) = Val $ i - j
sub e1 e2           = Sub e1 e2

bind (ID name) e = Bind name e
bind _         _ = undefined
