import Control.Concurrent.Chan (Chan(..), newChan, readChan, writeChan)
import Control.Concurrent (forkIO)

data CMsg = CDat String
          | CCtr (Chan Int)

main = do
  printerCh <- newChan
  ctrCh <- newChan
  forkIO $ printer printerCh
  mapM_
    (\i -> forkIO $ test i printerCh >> writeChan ctrCh 1)
    [1..mm]
  dropChan ctrCh mm
  writeChan printerCh (CCtr ctrCh)
  dropChan ctrCh 1
  where
    mm = 15


printer ch = do
  m <- readChan ch
  case m of
    CDat s ->  do
      putStrLn $ "Data: " ++ s
      printer ch
    CCtr ctrCh -> do
      putStrLn $ "Stop:."
      writeChan ctrCh 1

test i ch = do
  writeChan ch $ CDat (show i ++ " hi")
  writeChan ch $ CDat (show i ++ " cruel")
  writeChan ch $ CDat (show i ++ " world")

dropChan ch n = dropChan' ch 0 n --TODO as unfold?
  where
    dropChan' _ acc n | acc == n = return ()
    dropChan' ch acc n = do
      m <- readChan ch
      dropChan' ch (acc+1) n
