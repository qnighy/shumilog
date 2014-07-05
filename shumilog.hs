import qualified Lexer
import qualified Parser
import qualified Terms
import qualified Compile
import Text.Parsec
import System.IO

main :: IO ()
main = do
  handle <- openFile "sample.pl" ReadMode
  contents <- hGetContents handle
  case parse Parser.program "sample.pl" contents of
    Left err -> putStrLn $ show $ err
    Right program -> do
      putStrLn $ show $ program
      let (Parser.Program clauses _) = program
      let foo = Compile.compile clauses Terms.empty_env
      putStrLn "compiled."
