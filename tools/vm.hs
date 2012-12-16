{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Char
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

import Debug.Trace

type Address = Int32

type Memory = IOUArray Address Int32
type Output = IORef [Int32]

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
             word <- fromIntegral `fmap` getWord32le
             rest <- go
             return (word:rest)

run :: Integer -> Memory -> Address -> IO ()
run j memory ip = do

    let readMem offset = {-trace ("R " ++ show offset) $-} readArray memory offset
        writeMem offset value = writeArray memory offset value

    word <- readMem ip

    let hiword :: Int16
        hiword = fromIntegral $ word `shiftR` 16
        loword :: Int16
        loword = fromIntegral word .&. 0xffff

    let goNext = run (j+1) memory $ ip + fromIntegral loword

    -- printf "IP= 0x%x: hi=0x%x lo=%d (word: %x)\n" ip hiword loword word
    if hiword <= 8
       then do
            let op :: Int32 -> Int32 -> Int32
                op = case hiword of
                       1 -> (+)
                       2 -> (-)
                       3 -> (*)
                       4 -> div
                       5 -> (.&.)
                       6 -> (.|.)
                       7 -> (\x y -> x `shiftL` fromIntegral y)
                       8 -> (\x y -> x `shiftR` fromIntegral y)
                       _ -> error $ "Unknown opcode:" ++ show hiword
            offset <- readMem (ip + 2)
            x <- readMem (ip + offset)
            y <- readMem (ip + 3)
            dstOffset <- readMem (ip + 1)
            writeMem (ip + dstOffset) (x `op` y)
            goNext
       else case hiword of
              9 -> do
                   dstOffset <- readMem (ip + 1)
                   x <- readMem (ip + 2)
                   writeMem (ip + dstOffset) (complement x)
                   goNext
              10 -> do
                    offset <- readMem (ip + 1)
                    x <- readMem (ip + 2)
                    y <- readMem (ip + 3)
                    if x < y
                      then run (j+1) memory (ip + offset)
                      else goNext
              11 -> do
                    c <- readMem (ip + 1)
                    printf "%c" c
                    goNext
              _  -> goNext
              -- _  -> fail $ "Unknown opcode: " ++ show hiword

main = do
  [ips1, ips2] <- getArgs
  let ip1 = read ips1
      ip2 = read ips2
  fileData <- readBMP "../pic.bmp"
  forM_ [ip1 .. ip2] $ \ip -> do
      memory <- newListArray (0, 13371111-1) (fileData ++ repeat 0)
      putStrLn $ "IP=" ++ show ip
      run 0 memory ip
       `catch`
         (\(e :: SomeException) -> putStrLn $ "IP=" ++ show ip ++ ": Exception: " ++ show e )


