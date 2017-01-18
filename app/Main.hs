module Main where

import           Noroi.Client
import           Noroi.Context

test_connect :: IO (Client)
test_connect = do
  context <- Noroi.Context.newContext
  client <- Noroi.Client.newClient context "tcp://localhost:12345" "tcp://localhost:12346"
  return client

set_default_font :: Client -> IO ()
set_default_font client = do
  Noroi.Client.setFont client "/usr/share/noroi_test_server/font.ttf"

print_events :: Client -> IO ()
print_events client = do
  event <- Noroi.Client.handleEvent client
  case event of
    Nothing                   -> do
      print_events client
    Just (Noroi.Client.MouseMoveEvent x y) -> do
      Noroi.Client.text client x y "o"
      Noroi.Client.swapBuffers client
      print_events client
    Just Noroi.Client.QuitEvent ->
      putStrLn "Quiting"
    Just _                    -> do
      putStrLn $ show event
      print_events client

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

  -- Get the window size.
  (fw, fh) <- Noroi.Client.getSize client
  putStrLn $ "Got window size from the server: (" ++ show fw ++ ", " ++ show fh ++ ")"

  -- Drawing some text.
  Noroi.Client.text client 0 0 "Text from Haskell!"
  putStrLn "Drawing text at (0, 0)"

  -- Swap the buffer so that we can see our text.
  Noroi.Client.swapBuffers client
  putStrLn "Swapping Buffers"

  -- Print events
  print_events client

  -- Delete the client.
  Noroi.Client.deleteClient client
  putStrLn "Deleted a client!"

  -- Delete it.
  Noroi.Context.deleteContext context
  putStrLn "Deleted a context!"
