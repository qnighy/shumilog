import qualified Lexer
import qualified Parser
import qualified Terms
import Text.Parsec
import System.IO

main :: IO ()
main = do
  handle <- openFile "sample.pl" ReadMode
  contents <- hGetContents handle
  putStrLn $ show $ parse Parser.program "sample.pl" contents
