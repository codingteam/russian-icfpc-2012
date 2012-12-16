import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef
import qualified Data.IntMap as M
import qualified Data.ByteString as B
import Data.Word
import Data.Maybe
import Data.List
import System.Environment
import System.Random
import Text.Regex
import Codec.BMP -- from `bmp' package

--   send Value to process 348047,
--   [L,Q,V,I] <- receive(4),
--   P <- (L + Q + V + I + 2) / 4,
--   Value <- -16 * P / 64 + 28.
data Process = Process { pid :: Int
                       , sendList :: [Int]
                       , variables :: [Char]
                       , m :: Int  -- ^ multiplier
                       , s :: Int -- ^ end summand
                       , valueVar :: IORef Int
                       }

defaultProcess = Process { pid = 0
                         , sendList = []
                         , variables = []
                         , m = 1
                         , s = 0
                         }

type ChanArray = M.IntMap (TChan Int)

step :: ChanArray -> Process -> IO ()
step chans p = do
  value <- readIORef (valueVar p)
  forM_ (sendList p) $ \j -> do
    let Just to = M.lookup j chans
    atomically $ writeTChan to value
  let Just inbox = M.lookup (pid p) chans
  (a,b,c,d) <- atomically $ do
                 a' <- readTChan inbox
                 b' <- readTChan inbox
                 c' <- readTChan inbox
                 d' <- readTChan inbox
                 return (a', b', c', d')
  let t = (a + b + c + d + 2) `div` 4
      value = (m p) * t `div` 64 + (s p)
  writeIORef (valueVar p) value

launchProcesses :: TVar Int -> [Process] -> IO ()
launchProcesses counter ps = do
  chans <- replicateM (length ps) newTChanIO
  let chansMap = M.fromList $ zip (map pid ps) chans
  forM_ ps $ \p -> do
    putStrLn $ "Launching process " ++ show (pid p)
    forkIO $ do
        replicateM 7 $ step chansMap p
        atomically $ modifyTVar counter (+1)
        return ()

isProcess s   = "Process" `isPrefixOf` s
isSendValue s = "  send Value" `isPrefixOf` s
isVariables s = "  [" `isPrefixOf` s
isFormula s   = "  Value <-" `isPrefixOf` s

getSingle r s = head $ fromJust $ matchRegex (mkRegex r) s

getPid :: String -> Int
getPid s = read $ getSingle "Process ([0-9]+):" s

getSendPid :: String -> Int
getSendPid s = read $ getSingle "  send Value to process ([0-9]+)," s

getVariables :: String -> [Char]
getVariables s = map head (fromJust $ matchRegex (mkRegex "  \\[(.),(.),(.),(.)\\] <- receive\\(4\\),") s)

getMS s = map read (fromJust $ matchRegex (mkRegex "  Value <- (-?[0-9]+) \\* . / 64 \\+? ?(-?[0-9]+)\\.") s)

parse :: [String] -> [Process] -> [Process]
parse (x:xs) ps     | isProcess x =
                        parse (xs) (defaultProcess { pid = getPid x } : ps)
parse (x:xs) (p:ps) | isSendValue x =
                        let newPid = getSendPid x
                            process = p { sendList = newPid : sendList p }
                        in parse xs (process : ps)
parse (x:xs) (p:ps) | isVariables x =
                        let variables = getVariables x
                            process = p { variables = variables }
                        in parse xs (process : ps)
parse (x:xs) (p:ps) | isFormula x =
                        let [m, s] = getMS x
                            process = p { m = m, s = s }
                        in parse xs (process : ps)
parse (x : xs) ps = parse xs ps
parse [] ps       = ps

chop :: Int -> Word8
chop x
  | x <= 0    = 0
  | x >= 255  = 255
  | otherwise = fromIntegral x

showProcess process =
    "Process " ++ (show $ pid process) ++ " variables " ++ (show $ variables process) ++ " sendList " ++ (show $ sendList process) ++ " multiplier " ++ (show $ m process) ++ " summ " ++ (show $ s process)

main = do
  [filename] <- getArgs
  string <- readFile $ filename
  let ps = parse (lines string) []
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
              return [x, x, x, 0]
  let bitmap = B.pack $ concat pixels
      bmp    = packRGBA32ToBMP 1024 1024 bitmap
  writeBMP "output.bmp" bmp
