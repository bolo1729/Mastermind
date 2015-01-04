import Mastermind
import System.Environment (getArgs)

main = do
  getArgs >>= (putStr . unlines . map show . reverse . best . parseTurns)

