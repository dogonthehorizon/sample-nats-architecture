module Main where

import Data.Monoid ((<>))
import Data.Maybe (fromJust)
import Network.Nats (connect, publish, Nats)
import System.Environment as Env
import Control.Concurrent (threadDelay)
import System.IO (writeFile)
import Data.Time.LocalTime (getZonedTime)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = do
  host <- fromJust <$> Env.lookupEnv "SAMPLE_CLUSTER_SERVICE_HOST"
  port <- fromJust <$> Env.lookupEnv "SAMPLE_CLUSTER_SERVICE_PORT"
  ip   <- fromJust <$> Env.lookupEnv "POD_IP"
  nats <- connect ("nats://" <> host <> ":" <> port)
  forever nats ip
 where
   forever :: Nats -> String -> IO ()
   forever conn ip = do
     let msg = "Hello World, from " <> (BS.pack ip)
     publish conn "messages" msg

     BS.writeFile "/tmp/healthy" msg
     -- Wait 10 seconds before writing the healthy file again
     threadDelay 10000000
     forever conn ip
