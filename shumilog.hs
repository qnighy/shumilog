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

main :: IO ()
-- main = putStrLn "Hello"
-- main = do
--   handle <- openFile "sample.pl" ReadMode
--   contents <- hGetContents handle
--   case parse Parser.program "sample.pl" contents of
--     Left err -> putStrLn $ show $ err
--     Right program -> do
--       putStrLn $ show $ program
--       let (Parser.Program clauses queries') = program
--       let env = Compile.compile clauses Terms.empty_env
--       putStrLn "compiled."
--       forM_ (Map.assocs $ Terms.predicateDecls env) $ \(sym,cls) ->
--         forM_ cls $ \cl ->
--           putStrLn $ runReader (Terms.showClause sym cl) env
--       forM_ queries' $ \query ->
--         case runErrorT $ flip runReaderT env $ do
--           cquery <- Compile.compileQuery query
--           Eval.evalQuery cquery
--         of
--           Left msg -> putStrLn $ "error: " ++ msg
--           Right (Left msg) -> putStrLn $ "error: " ++ msg
--           Right (Right ms) -> do
--             if null ms then
--               putStrLn "false."
--             else
--               forM_ ms $ \m ->
--                 if Map.null m then
--                   putStrLn "true."
--                 else
--                   forM_ (Map.assocs m) $ \(k,v) ->
--                     putStrLn $ show k ++ " : "
--                                  ++ flip runReader env (Terms.showTerm v)
--             putStrLn ""

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

-- type M = StateT Environment (ListT (ErrorT String IO))

main = do
  handle <- openFile "sample.pl" ReadMode
  contents <- hGetContents handle
  let res' = runParserT readEvalF () "sample.pl" contents
  res <- flip evalStateT empty_env $ runErrorT $ res'
  case res of
    Left err -> putStrLn $ show $ err
    Right (Left err) -> putStrLn $ show $ err
    Right (Right _) -> return ()
  -- case parse Parser.program "sample.pl" contents of
  --   Left err -> putStrLn $ show $ err
  --   Right program -> do
  --     putStrLn $ show $ program
  --     let (Parser.Program clauses queries') = program
  --     let env = Compile.compile clauses Terms.empty_env
  --     putStrLn "compiled."
  --     forM_ (Map.assocs $ Terms.predicateDecls env) $ \(sym,cls) ->
  --       forM_ cls $ \cl ->
  --         putStrLn $ runReader (Terms.showClause sym cl) env
  --     forM_ queries' $ \query ->
  --       case runErrorT $ flip runReaderT env $ do
  --         cquery <- Compile.compileQuery query
  --         Eval.evalQuery cquery
  --       of
  --         Left msg -> putStrLn $ "error: " ++ msg
  --         Right (Left msg) -> putStrLn $ "error: " ++ msg
  --         Right (Right ms) -> do
  --           if null ms then
  --             putStrLn "false."
  --           else
  --             forM_ ms $ \m ->
  --               if Map.null m then
  --                 putStrLn "true."
  --               else
  --                 forM_ (Map.assocs m) $ \(k,v) ->
  --                   putStrLn $ show k ++ " : "
  --                                ++ flip runReader env (Terms.showTerm v)
  --           putStrLn ""
