import System.Environment

data Process = Process { id :: Integer
                       , sendList :: [Integer]
                       , variables :: [Char]
                       , m :: Integer
                       , v :: Char
                       , s :: Integer }

step process mailbox =
    

main = do
  [filename] <- getArgs
  return 0