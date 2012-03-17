module Main where
import Data.ByteString as B
import Data.ByteString.UTF8
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

salas = [sala1,sala2,sala4]

-- from http://www.brainless.us/forum/viewtopic.php?f=7&t=57
query1 = pack [0xFF, 0xFF, 0xFF, 0XFF, 0x67, 0x65, 0x74, 0x69, 0x6E, 0x66, 0x6F, 0x20, 0x78,0x78,0x78]
query2 = pack [0xFF, 0xFF, 0xFF, 0XFF, 0x67, 0x65, 0x74, 0x73, 0x74, 0x61, 0x74, 0x75, 0x73]

queryServer :: (String,PortNumber) -> IO ByteString
queryServer (ip,port) = do sock <- socket AF_INET Datagram defaultProtocol
                           addr <- inet_addr ip
                           sendTo sock query2 (SockAddrInet port addr)
                           recv sock 10000

pp r = let l = B.split 0xa r
           m = List.map toString $ B.split 0x5c $ l !! 1
           findNext s = m !! ((fromJust $ List.findIndex (==s) m)+1) 
           sv_hostname = findNext "sv_hostname"
           mapname = findNext "mapname"
           f x | B.length x > 0 = toString $ List.last $ B.split (fromIntegral (ord ' ')) x
               | otherwise = ""
           players = List.map f (List.drop 2 l)

       in  "sv_hostname: " ++ sv_hostname ++ "\n" ++
           "mapname: " ++ mapname ++ "\n" ++
           "players: " ++ (List.concat $ List.intersperse ", " players) ++ "\n"

hexdump = Prelude.map (\x-> ((flip showHex "")x,chr (fromIntegral x)) ) . unpack

main = mapM_ (\x->(liftM pp) (queryServer x) >>= Prelude.putStrLn) salas
