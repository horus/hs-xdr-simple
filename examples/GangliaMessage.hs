module GangliaMessage where

import           Codec.Binary.XDR.Simple
import qualified Data.ByteString.Lazy           as LBS
import           Data.Int                       (Int32)
import           Network.Socket                 hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString.Lazy as LBS

test :: IO ()
test = do
    (serveraddr:_) <- getAddrInfo Nothing (Just "127.0.0.1") (Just "8655")
    sock           <- socket (addrFamily serveraddr) Datagram defaultProtocol
    connect sock (addrAddress serveraddr)
    LBS.send sock $ ganglia31MetaMessage "ganglia.example.net" "haskell.test"
                        "100.23" "u/L" 3 60 0 [("abc", "def"), ("键", "值")]
    LBS.send sock $ ganglia31MetricStringMessage "ganglia.example.net" "haskell.test.测试"
                        "%.f" "100.23"
    close sock

vType :: String -> String
vType val
    | null asFloat = "string"
    | otherwise    = "float"
    where asFloat :: [(Float, String)]
          asFloat = reads val

-- ganglia source: lib/gm_protocol.x

ganglia31MetricId :: String
                  -> String
                  -> Int32
                  -> Pack
ganglia31MetricId host name spoof = do
    pack host
    pack name
    pack spoof

ganglia31MetadataMessage :: String  -- type
                         -> String  -- name
                         -> String  -- units
                         -> Int32   -- slope
                         -> Int32   -- tmax
                         -> Int32   -- dmax
                         -> [(String, String)] -- extra
                         -> Pack
ganglia31MetadataMessage typ name units slope tmax dmax extra = do
    pack typ
    pack name
    pack units
    pack slope
    pack tmax
    pack dmax
    pack (fromIntegral (length extra) :: UInt)
    mapM_ putKeyVal extra
    where putKeyVal :: (String, String) -> Pack
          putKeyVal (k, v) = pack k >> pack v

ganglia31MetaMessage :: String  -- host
                     -> String  -- name
                     -> String  -- value
                     -> String  -- units
                     -> Int32   -- slope
                     -> Int32   -- tmax
                     -> Int32   -- dmax
                     -> [(String, String)] -- extra
                     -> LBS.ByteString
ganglia31MetaMessage host name value units slope tmax dmax extra = xdrPack packs
    where packs = do pack (128 :: Int32) -- Ganglia_msg_formats: gmetadata_full
                     ganglia31MetricId host name 0
                     ganglia31MetadataMessage (vType value) name units slope tmax dmax extra

ganglia31MetricStringMessage :: String
                             -> String
                             -> String
                             -> String -- format
                             -> LBS.ByteString
ganglia31MetricStringMessage host name format value = xdrPack packs
    where packs = do pack (133 :: Int32) -- Ganglia_msg_formats: gmetric_string
                     ganglia31MetricId host name 0
                     pack format
                     pack value
