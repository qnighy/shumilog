{-# LANGUAGE FlexibleContexts #-}
import Lexer
import Parser
import Terms
import Preterm
import Compile
import Eval
import Text.Parsec
import qualified Data.Map.Strict as Map
import Control.Applicative ((*>))
import Control.Monad.State.Strict
import Control.Monad.Error
import System.IO
import System.Environment


readEvalF' :: Stream s M Char => ParsecT s u M ()
readEvalF' = do
  stmtOrEnd <- statementOrEnd
  case stmtOrEnd of
    Just stmt -> do
      lift $ compileStatement stmt
      readEvalF
    Nothing -> return ()

readEvalF :: Stream s M Char => ParsecT s u M ()
readEvalF = whiteSpace *> readEvalF'

main :: IO ()
main = do
  args <- getArgs
  let fname = case args of { (s:_) -> s ; [] -> "sample.pl" }
  handle <- openFile fname ReadMode
  contents <- hGetContents handle
  let res' = runParserT readEvalF () fname contents
  res <- flip evalStateT empty_env $ runErrorT $ res'
  case res of
    Left err -> putStrLn $ show $ err
    Right (Left err) -> putStrLn $ show $ err
    Right (Right _) -> return ()
