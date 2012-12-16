import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
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
                       }

type ChanArray = M.IntMap (TChan Int)

step :: ChanArray -> Process -> IO ()
step chans p = do
  let Just inbox = M.lookup (pid p) chans
  (a,b,c,d) <- atomically $ do
                 a' <- readTChan inbox
                 b' <- readTChan inbox
                 c' <- readTChan inbox
                 d' <- readTChan inbox
                 return (a', b', c', d')
  let t = (a + b + c + d + 2) `div` 4
      value = (m p) * t `div` 64 + (s p)
  forM_ (sendList p) $ \j -> do
    let Just to = M.lookup j chans
    atomically $ writeTChan to value

isProcess s = "Process" `isPrefixOf` s

getPid :: String -> Int
getPid s = read $ (fromJust $ matchRegex (mkRegex "Process ([0-9]+):") s) !! 0

parse :: [String] -> [Process] -> [Process]
parse (x : xs) ps | isProcess x =
                      parse (xs) (Process { pid = getPid x } : ps)
parse _ ps = ps

main = do
  [filename] <- getArgs
  string <- readFile $ filename
  processes = parse $ lines string
  return $ length processes
