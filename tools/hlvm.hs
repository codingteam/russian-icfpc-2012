import System.Environment

data Process = Process { id :: Integer
                       , sendList :: [Integer]
                       , variables :: [Char]
                       , m :: Integer
                       , v :: Char
                       , s :: Integer }

main = do
  [filename] <- getArgs
  return 0