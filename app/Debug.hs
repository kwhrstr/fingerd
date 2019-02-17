module Main where

import Control.Monad (forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import GHC.IO (bracket)



logAndEcho :: Socket -> IO ()
logAndEcho sock = forever $
  bracket
    (fst <$> accept sock)
    close
    printAndKickback
  where
    printAndKickback conn = do
      msg <- recv conn 1024
      print msg
      sendAll conn msg
      
main :: IO ()
main = withSocketsDo $ do
  addInfos <- getAddrInfo
                (Just defaultHints
                       {addrFlags = [AI_PASSIVE]})
                Nothing
                (Just "79")
  let severaddr = head addInfos
  bracket
    (socket (addrFamily severaddr) Stream defaultProtocol)
    close
    (\sock -> do
      bind sock (addrAddress severaddr)
      listen sock 1
      logAndEcho sock)
