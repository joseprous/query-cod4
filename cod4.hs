module Main where
import qualified Data.ByteString as B
import Data.ByteString.UTF8 (ByteString,toString)
import Network.Socket.ByteString
import Network.Socket hiding (sendTo,recv)
import Char
import Numeric
import Maybe
import List
import Monad

sala1 = ("190.211.243.238",28958)
sala2 = ("190.211.243.238",28959)
sala4 = ("190.211.243.238",28961)

servers = [sala1,sala2,sala4]

-- from http://www.brainless.us/forum/viewtopic.php?f=7&t=57
query1 = B.pack [0xFF, 0xFF, 0xFF, 0XFF, 0x67, 0x65, 0x74, 0x69, 0x6E, 0x66, 0x6F, 0x20, 0x78,0x78,0x78]
query2 = B.pack [0xFF, 0xFF, 0xFF, 0XFF, 0x67, 0x65, 0x74, 0x73, 0x74, 0x61, 0x74, 0x75, 0x73]

queryServer :: (String,PortNumber) -> IO ByteString
queryServer (ip,port) = do sock <- socket AF_INET Datagram defaultProtocol
                           addr <- inet_addr ip
                           sendTo sock query2 (SockAddrInet port addr)
                           recv sock 10000

ordB = fromIntegral . ord

splitC :: Char -> ByteString -> [ByteString]
splitC = B.split . ordB

pp :: ByteString -> [Char]
pp rawData = let parts = splitC '\n' rawData
                 info = map toString $ splitC '\\' $ parts !! 1
                 getValue key = key ++ ": " ++ value
                          where value = info !! ((fromJust $ findIndex (==key) info)+1)

                 getPlayer = toString . B.dropWhile (\c->c /= ordB '"')
                 players = map getPlayer (drop 2 parts)

             in  getValue "sv_hostname" ++ "\n" ++
                 getValue "mapname" ++ "\n" ++
                 "players: " ++ (concat $ intersperse ", " players) ++ "\n"

main = mapM_ (\x->(liftM pp) (queryServer x) >>= putStrLn) servers
