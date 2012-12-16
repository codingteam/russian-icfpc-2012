import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef
import qualified Data.IntMap as M
import System.Environment

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

main = do
  print "Hello world!"
