module Main where

import Noroi.Client
import Noroi.Context

main :: IO ()
main = do
  -- Make a context to do stuff with.
  context <- Noroi.Context.newContext
  putStrLn "Created a context!"

  -- Create a Client
  client <- Noroi.Client.newClient context "tcp://localhost:12345" "tcp://localhost:12346"
  putStrLn "Created a client!"

  -- Change the caption to something.
  Noroi.Client.setCaption client "Haskell Caption!"
  putStrLn "Setting caption."

  -- Change the font.
  Noroi.Client.setFont client "/usr/share/noroi_test_server/font.ttf"
  putStrLn "Setting font."

  -- Set the font size.
  Noroi.Client.setSize client 30 30
  putStrLn "Set window size to (30, 30)"

  -- Get the font size.
  (fw, fh) <- Noroi.Client.getSize client
  putStrLn $ "Got window size from the server: (" ++ show fw ++ ", " ++ show fh ++ ")"

  -- Drawing some text.
  Noroi.Client.text client 0 0 "Text from Haskell!"
  putStrLn "Drawing text at (0, 0)"

  -- Delete the client.
  Noroi.Client.deleteClient client
  putStrLn "Deleted a client!"

  -- Delete it.
  Noroi.Context.deleteContext context
  putStrLn "Deleted a context!"
