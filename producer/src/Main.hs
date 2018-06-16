module Main where

import Data.Monoid ((<>))
import Data.Maybe (fromJust)
import Network.Nats (connect, subscribe, NatsSID)
import System.Environment as Env
import Control.Concurrent (threadDelay)
import System.IO (writeFile)
import Data.Time.LocalTime (getZonedTime)

main :: IO ()
main = do
  host <- fromJust <$> Env.lookupEnv "COHEE_CLUSTER_SERVICE_HOST"
  port <- fromJust <$> Env.lookupEnv "COHEE_CLUSTER_SERVICE_PORT"
  ip   <- fromJust <$> Env.lookupEnv "POD_IP"
  nats <- connect ("nats://" <> host <> ":" <> port)

  print ("Subscribing to nats queue: " <> host <> ":" <> port)
  natsId <- subscribe nats "messages" Nothing $
    \_ _ msg _ -> do    -- The parameters are (sid, subject, message, reply_subject)
      now <- getZonedTime
      putStrLn $ show now <> " | " <> ip <> ": " <> show msg

  -- We must loop infinitely because otherwise `subscribe` returns immediately
  -- and the container exits. This is not the behavior we want!
  loopUntilFailure natsId
 where
   loopUntilFailure :: NatsSID -> IO ()
   loopUntilFailure id = do
     writeFile "/tmp/healthy" $ show id
     -- Wait 10 seconds before writing the healthy file again
     threadDelay 10000000
     loopUntilFailure id
