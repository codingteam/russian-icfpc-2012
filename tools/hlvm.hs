import Control.Concurrent.STM.TChan
import System.Environment

data Process = Process { id :: Integer
                       , sendList :: [Integer]
                       , variables :: [Char]
                       , m :: Integer
                       , v :: Char
                       , s :: Integer
                       , value :: Integer }

send processId value = undefined

step process chan = do
  mapM (\id -> send id value) sendList
  values <- mapM read [1..4] chan
  process { value = getValue values }
    where getValue vs = (m process) * result / 64 + (s process)
          result   vs = ((sum vs) + 2) / 4


main = do
  [filename] <- getArgs
  return 0