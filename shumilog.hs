import qualified Lexer
import qualified Parser
import qualified Terms
import qualified Compile
import qualified Eval
import Text.Parsec
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Error
import System.IO

main :: IO ()
main = do
  handle <- openFile "sample.pl" ReadMode
  contents <- hGetContents handle
  case parse Parser.program "sample.pl" contents of
    Left err -> putStrLn $ show $ err
    Right program -> do
      putStrLn $ show $ program
      let (Parser.Program clauses queries') = program
      let env = Compile.compile clauses Terms.empty_env
      putStrLn "compiled."
      forM_ (Map.assocs $ Terms.predicateDecls env) $ \(sym,cls) ->
        forM_ cls $ \cl ->
          putStrLn $ runReader (Terms.showClause sym cl) env
      forM_ queries' $ \query ->
        case runErrorT $ flip runReaderT env $ do
          cquery <- Compile.compileQuery query
          Eval.evalQuery cquery
        of
          Left msg -> putStrLn $ "error: " ++ msg
          Right (Left msg) -> putStrLn $ "error: " ++ msg
          Right (Right ms) -> do
            if null ms then
              putStrLn "false."
            else
              forM_ ms $ \m ->
                if Map.null m then
                  putStrLn "true."
                else
                  forM_ (Map.assocs m) $ \(k,v) ->
                    putStrLn $ show k ++ " : "
                                 ++ flip runReader env (Terms.showTerm v)
            putStrLn ""
