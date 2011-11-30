import Network (listenOn, withSocketsDo, accept, PortID(..), Socket, Socket.HostName, Socket.PortNumber)
import System (getArgs)
import System.Log.Logger
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO, myThreadId)

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = fromIntegral (read $ head args :: Int)
    sock <- listenOn $ PortNumber port
    debugM "Troyd.OApi" $ "Listening on " ++ (head args)
    acceptor sock

acceptor :: Socket -> IO ()
acceptor sock = do
    putStrLn "Accepting..."
    (handle, host, port) <- accept sock
    putStrLn $ "Accepted " ++ (show host) ++ (show port)
    --hSetBuffering handle NoBuffering
    hSetBuffering handle LineBuffering
    threadId <- forkIO $ (handler handle host port)
    putStrLn $ "Created handler " ++ (show threadId)
    acceptor sock

handler :: Handle -> HostName -> PortNumber -> IO ()
handler handle host port = do
    threadId <- myThreadId
    putStrLn $ "Handler " ++ (show threadId) ++ " processing  connection " ++ (show host) ++ (show port)
    line <- hGetLine handle
    -- orderReq from line
    -- lookup book Chan (createOrder) and forward order+clientRef to book thread
    -- this thread (clientRef) keep connection open to
    -- recv further req from client and send message responds from book
    hPutStrLn handle "got it"
    handler handle host port

