import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef
import qualified Data.IntMap as M
import Data.Maybe
import Data.List
import System.Environment
import Text.Regex

--   send Value to process 348047,
--   [L,Q,V,I] <- receive(4),
--   P <- (L + Q + V + I + 2) / 4,
--   Value <- -16 * P / 64 + 28.
data Process = Process { pid :: Int
                       , sendList :: [Int]
                       , variables :: [Char]
                       , m :: Int  -- ^ multiplier
                       , v :: Char
                       , s :: Int -- ^ end summand
                       , valueVar :: IORef Int
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

isProcess s = "Process" `isPrefixOf` s
isSendValue s = "send Value" `isPrefixOf` s
isVariables s = "[" `isPrefixOf` s
isFormula s = "Value <-" `isPrefixOf` s

getSingle r s = (fromJust $ matchRegex (mkRegex r) s) !! 0

getPid :: String -> Int
getPid s = read $ getSingle "Process ([0-9]+):" s

getSendPid :: String -> Int
getSendPid s = read $ getSingle "send Value to process ([0-9]+)," s

getVariables :: String -> [Char]
getVariables s = map read (fromJust $ matchRegex (mkRegex "[(.),(.),(.),(.)] <- receive(4),") s)

getMS s = map read (fromJust $ matchRegex (mkRegex "Value <- (-?[0-9+]) * . / 64 +? ?(-?[0-9]+)\\.") s)

parse :: [String] -> [Process] -> [Process]
parse (x:xs) ps   | isProcess x =
                      parse (xs) (Process { pid = getPid x } : ps)
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

main = do
  [filename] <- getArgs
  string <- readFile $ filename
  putStrLn $ show $ length $ parse (lines string) []
