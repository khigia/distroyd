import System (getArgs)
import System.Log.Logger
import System.Log.Formatter (tfLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.IO (stderr)
import Control.Concurrent (forkIO)

import qualified Troyd.OApi
import qualified Troyd.MApi
import qualified Troyd.Exch

logger = "Troyd.Main"

main :: IO ()
main = do
  --TODO check what's an FRP approach would looks like
  --TODO clean stop
  --TODO exception handling
  args <- getArgs -- TODO command arg parser
  setupLogging
  let port = fromIntegral (read $ head args :: Int)
  exch <- Troyd.Exch.expMake ["A", "ZZZZ"]
  forkIO $ Troyd.MApi.server (port + 1) (mktNewClient exch)
  Troyd.OApi.server port (omNewClient exch)
  where
    omNewClient exch host port recvThreadId sender = do
      cli <- Troyd.Exch.clMake host port sender
      return $ ack cli
      where
        ack cli m =
          debugM logger ("Received:" ++ show m)
          >> Troyd.Exch.expOrdReq exch cli m
          >>= Troyd.Exch.clSend cli
    mktNewClient exp slot =
      Troyd.Exch.expMktSubscribe exp slot
      >>= slot

setupLogging = do
  let logFormatter = tfLogFormatter "%Y-%m-%d %H:%M:%S%Q" "$time $loggername $prio $pid $tid $msg"
  logHandler <-
    streamHandler stderr DEBUG
    >>= \h -> return $ setFormatter h logFormatter
  updateGlobalLogger rootLoggerName (setHandlers [logHandler])
  updateGlobalLogger rootLoggerName (setLevel DEBUG)

