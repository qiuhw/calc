import Control.Monad
import Control.Monad.State (runState)
import qualified Data.Map as Map
import System.IO
import System.IO.Error (catchIOError)

import Calc.SyntaxTree (Environment, Expr(..), evalExpr)
import Calc.Parser (parseStmt)

main = do
    hSetBuffering stdout NoBuffering
    mainLoop Map.empty

mainLoop :: Environment -> IO ()
mainLoop env = do
    putStr "> "
    catchIOError (getLine >>= mainLoop' env) (\_ -> return ())

mainLoop' :: Environment -> String -> IO ()
mainLoop' env s = do
    (str, env') <- case parseStmt s of
        Left e     -> print e >> return (Nil, env)
        Right expr -> return $ runState (evalExpr expr) env
    print str
    mainLoop env'
