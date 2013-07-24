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

evalExpr (Negative (Val i)) = return . Val $ -i
evalExpr (Negative e)       = liftM Negative $ evalExpr e

evalExpr (Pow (Val i) (Val j)) = return . Val $ i^j
evalExpr (Pow e1 e2)           = liftM2 Pow (evalExpr e1) (evalExpr e2)

evalExpr (Mul (Val i) (Val j)) = return . Val $ i*j
evalExpr (Mul e1 e2)           = liftM2 Mul (evalExpr e1) (evalExpr e2)

evalExpr (Div (Val i) (Val j)) = return . Val $ i `div` j
evalExpr (Div e1 e2)           = liftM2 Div (evalExpr e1) (evalExpr e2)

evalExpr (Add (Val i) (Val j)) = return . Val $ i+j
evalExpr (Add e1 e2)           = liftM2 Add (evalExpr e1) (evalExpr e2)

evalExpr (Sub (Val i) (Val j)) = return . Val $ i-j
evalExpr (Sub e1 e2)           = liftM2 Sub (evalExpr e1) (evalExpr e2)

evalExpr (Bind name e) = do
    expr <- evalExpr e
    modify $ insert name expr
    return Nil
