{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef
import Data.Array
import qualified Data.ByteString as B
import qualified Data.IntMap as M
import Data.Word
import Data.Maybe
import Data.List
import System.Environment
import System.Random
import Data.Attoparsec hiding (skipWhile)
import Data.Attoparsec.Char8
import Data.Attoparsec.Combinator
import Codec.BMP -- from `bmp' package

--   send Value to process 348047,
--   [L,Q,V,I] <- receive(4),
--   P <- (L + Q + V + I + 2) / 4,
--   Value <- -16 * P / 64 + 28.
data Process = Process { pid :: Int
                       , sendList :: [Int]
                       , m :: Int  -- ^ multiplier
                       , s :: Int -- ^ end summand
                       , valueVar :: IORef Int
                       }

defaultProcess = Process { pid = 0
                         , sendList = []
                         , m = 1
                         , s = 0
                         }

type ChanArray = Array Int (TChan Int)

step :: ChanArray -> Process -> Int -> IO ()
step chans p i = do
  let log str = putStrLn $ "Process " ++ show (pid p) ++ ", iteration #" ++ show i ++ ": " ++ str
  log "Starting"
  value <- readIORef (valueVar p)
--   log $ "Value read: " ++ show value
  forM_ (sendList p) $ \j -> do
    let to = chans ! j
--     log $ "Sending value to process " ++ show j
    atomically $ writeTChan to value
--   log "Values sent"
  let inbox = chans ! pid p
  (a,b,c,d) <- atomically $ do
                 a' <- readTChan inbox
                 b' <- readTChan inbox
                 c' <- readTChan inbox
                 d' <- readTChan inbox
                 return (a', b', c', d')
--   log "Variables read"
  let t = (a + b + c + d + 2) `div` 4
      value = (m p) * t `div` 64 + (s p)
  writeIORef (valueVar p) value
--   log "Value wrote"

sequencel xs = foldl k (\r -> return $ r []) xs id
    where
      k g m = \r -> do
        x <- m
        g (r . (x:))

launchProcesses :: TVar Int -> [Process] -> IO ()
launchProcesses counter ps = do
  putStrLn $ "Creating " ++ show (length ps) ++ " Chans."
  chans <- sequencel $ replicate (length ps) newTChanIO
  putStrLn "Channels created."
  let chansMap = array (0, 1024*1024-1) $ zip (map pid ps) chans
  forM_ (zip [1..] ps) $ \(i, p) -> do
    putStrLn $ "Launching process #" ++ show i ++ ": " ++ show (pid p)
    forkIO $ do
        forM_ [1..7] $ \i -> step chansMap p i
        atomically $ modifyTVar counter (+1)
        return ()

send = do
  string "  send Value to process "
  n <- decimal
  string ",\n"
  return n

parser :: Parser Process
parser = do
  string "Process "
  p <- decimal
  string ":"
  endOfLine
  slist <- many' send
  string "  ["
  skipWhile (/= '\n')
  string "\n  "
  letter_ascii
  string " <- ("
  skipWhile (/= '\n')
  string "\n  Value <- "
  m <- option 1 $ do
         m' <- signed decimal
         string " * "
         return m'
  letter_ascii
  string " / 64"
  many' space
  c <- anyChar
  case c of
   '.' -> do
          many1 endOfLine
          return $ Process {pid = p, sendList = slist, m = m, s = 0}
   '-' -> do
          s <- decimal
          string "."
          many1 endOfLine
          return $ Process {pid = p, sendList = slist, m = m, s = negate s}
   '+' -> do
          space
          s <- decimal
          string "."
          many1 endOfLine
          return $ Process {pid = p, sendList = slist, m = m, s = s}
   _ -> fail $ "Unexpected " ++ [c]

chop :: Int -> Word8
chop x
  | x <= 0    = 0
  | x >= 255  = 255
  | otherwise = fromIntegral x

showProcess process =
    "Process " ++ (show $ pid process) ++ " sendList " ++ (show $ sendList process) ++ " multiplier " ++ (show $ m process) ++ " summ " ++ (show $ s process)

-- main = do
--   content <- B.readFile "../virtual-machine/out1"
--   let x = case parseOnly parser content of
--             Right res -> res
--             Left err -> error err
--   putStrLn $ showProcess x

main = do
  [filename] <- getArgs
  content <- B.readFile filename
  let ps = case parseOnly (many1 parser) content of
             Left err -> error err
             Right res -> res
  putStrLn $ show $ length ps
  processes <- forM ps $ \p -> do
                  rnd <- randomRIO (0, 255)
                  var <- newIORef rnd
                  return $ p {valueVar = var}
  putStrLn $ showProcess $ head processes

  putStrLn "IORefs created."
  counter <- newTVarIO 0
  launchProcesses counter processes
  putStrLn "Processes launched."

  -- wait for all processes
  atomically $ do
    count <- readTVar counter
    if count < length processes
      then retry
      else return ()

  let psMap = M.fromList [(pid p, valueVar p) | p <- processes]
  pixels <- forM [0 .. 1024*1024 - 1] $ \j -> do
              let Just var = M.lookup j psMap
              result <- readIORef var
              let x = chop result
              return [x, x, x, 255]
  let bitmap = B.pack $ concat pixels
      bmp    = packRGBA32ToBMP 1024 1024 bitmap
  writeBMP "output.bmp" bmp
