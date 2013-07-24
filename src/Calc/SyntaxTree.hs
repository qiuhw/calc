module Calc.SyntaxTree
    ( Environment
    , Expr(..)
    , evalExpr
    ) where

import Control.Monad (liftM, liftM2)
import Control.Monad.State (State, get, modify)
import Data.Map (Map, insert, lookup)

import Prelude hiding (lookup)

type Environment = Map String Expr

data Expr = Nil
          | Val Integer
          | ID String
          | Negative Expr
          | Pow Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Bind String Expr

instance Show Expr where
    show Nil          = ""
    show (Val i)      = show i
    show (ID s)       = s
    show (Negative e) = '-' : parens e
    show (Pow e1 e2)  = parens e1 ++ "^" ++ parens e2
    show (Mul e1 e2)  = parens e1 ++ "*" ++ parens e2
    show (Div e1 e2)  = parens e1 ++ "/" ++ parens e2
    show (Add e1 e2)  = parens e1 ++ " + " ++ parens e2
    show (Sub e1 e2)  = parens e1 ++ " - " ++ parens e2
    show (Bind s e)   = s ++ "=" ++ show e

parens Nil            = ""
parens (Val i)        = show i
parens (ID s)         = s
parens e              = '(' : show e ++ ")"

evalExpr :: Expr -> State Environment Expr
evalExpr Nil = return Nil

evalExpr e@(Val _) = return e

evalExpr e@(ID name)  = do
    m <- get
    case lookup name m of
        Nothing  -> return e
        Just exp -> evalExpr exp

evalExpr (Negative e) = liftM neg $ evalExpr e
  where neg (Val i) = Val $ -i
        neg e       = Negative e

evalExpr (Pow e1 e2) = liftM2 pow (evalExpr e1) (evalExpr e2)
  where pow (Val i) (Val j) = Val $ i^j
        pow e1 e2           = Pow e1 e2

evalExpr (Mul e1 e2) = liftM2 mul (evalExpr e1) (evalExpr e2)
  where mul (Val i) (Val j) = Val $ i*j
        mul e1 e2           = Mul e1 e2

evalExpr (Div e1 e2) = liftM2 iDiv (evalExpr e1) (evalExpr e2)
  where iDiv (Val i) (Val j) = Val $ i `div` j
        iDiv e1 e2           = Div e1 e2

evalExpr (Add e1 e2) = liftM2 add (evalExpr e1) (evalExpr e2)
  where add (Val i) (Val j) = Val $ i+j
        add e1 e2           = Add e1 e2

evalExpr (Sub e1 e2) = liftM2 sub (evalExpr e1) (evalExpr e2)
  where sub (Val i) (Val j) = Val $ i-j
        sub e1 e2           = Sub e1 e2

evalExpr (Bind name e) = do
    expr <- evalExpr e
    modify $ insert name expr
    return Nil
