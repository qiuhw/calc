module Calc.Parser (parseStmt) where

import Control.Monad (liftM)
import Text.Parsec (ParseError, (<|>), (<?>), char, parse, spaces)
import Text.Parsec.Expr (Assoc(..), Operator(..), buildExpressionParser)
import qualified Text.Parsec.Token as Parsec
import Text.Parsec.Language (haskellStyle)

import Calc.SyntaxTree (Expr(..))
import Calc.Op (neg, pow, mul, iDiv, add, sub, bind)

parseStmt :: String -> Either ParseError Expr
parseStmt = parse expr "(unknown)"

expr =  buildExpressionParser table term
    <?> "expression"

term =  parens expr
    <|> liftM ID identifier
    <|> liftM Val natural
    <?> "simple expression"

table = [ [prefix "-" neg, prefix "+" id ]
        , [binary "^" pow AssocRight ]
        , [binary "*" mul AssocLeft, binary "/" iDiv AssocLeft ]
        , [binary "+" add AssocLeft, binary "-" sub AssocLeft ]
        , [binary "=" bind AssocRight ]
        ]

prefix name fun  = Prefix (reservedOp name >> return fun )
postfix name fun = Postfix (reservedOp name >> return fun)
binary name fun  = Infix (reservedOp name >> return fun )

lexer = Parsec.makeTokenParser haskellStyle

parens     = Parsec.parens lexer
identifier = Parsec.identifier lexer
natural    = Parsec.natural lexer
reservedOp = Parsec.reservedOp lexer
