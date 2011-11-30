import qualified Data.ByteString.Lazy as ByteString (
       readFile, writeFile, length)

import Troyd.Protbuf.ReqOrderAdd

import Text.ProtocolBuffers.Header (uFromString)
import Text.ProtocolBuffers.WireMessage (messageGet, messagePut)

main =
  let m = ReqOrderAdd { instrument = uFromString "ZZZZ" }
  in do
    putStrLn "Public cheri, mon amour!"
    ByteString.writeFile "test.pb" $ messagePut m
