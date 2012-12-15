{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.Word
import Data.Int
import Data.Bits
import Data.Array.IO
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
-- import Graphics.UI.SDL
-- import Graphics.UI.SDL
import System.IO
import System.Environment
import Text.Printf

type Address = Int32

type Memory = IOUArray Address Int32

readBMP :: FilePath -> IO [Int32]
readBMP path = do
    content <- L.readFile path
    return $ runGet (skip 54 >> go) content
  where
    go = do
      end <- isEmpty
      if end
        then return []
        else do
             word <- get
             rest <- go
             return (word:rest)

run :: Memory -> Address -> IO ()
run memory ip = do
  word <- readArray memory ip
  let hiword :: Int16
      hiword = fromIntegral $ word `shiftR` 16
      loword :: Int16
      loword = fromIntegral word .&. 0xffff
  printf "IP= 0x%x: hi=0x%x lo=%d (word: %x)\n" ip hiword loword word
  if loword == 0
    then putStrLn "Lo = 0, stop."
    else run memory $ ip + fromIntegral loword

main = do
  [ips] <- getArgs
  let ip = read ips
  fileData <- readBMP "pic.bmp"
  memory <- newListArray (0, 1337111-1) (fileData ++ repeat 0)
  -- forM_ fileData print
  run memory ip
