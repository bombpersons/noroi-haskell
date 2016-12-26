{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Noroi.Client (
  newClient,
  deleteClient,
  setFont,
  setFontSize,

  setSize,
  getSize,

  setCaption,

  text,

  Client(..)
) where

import Noroi.Context

import Foreign
import Foreign.C.String
import Foreign.Ptr
import Foreign.C.Types

newtype Client = Client (Ptr Client)

#include <noroi/client/noroi_client.h>

-- Create a new client.
newClient :: Context -> String -> String -> IO Client
newClient context r s = withCString r $ \cr -> withCString s $ \cs -> c_clientNew context cr cs
foreign import ccall unsafe "NR_Client_New" c_clientNew :: Context -> CString -> CString -> IO Client

-- Delete a client..
deleteClient = c_clientDelete
foreign import ccall unsafe "NR_Client_Delete" c_clientDelete :: Client -> IO ()

-- Set the font.
setFont :: Client -> String -> IO ()
setFont client f = withCString f $ \cf -> c_clientSetFont client cf
foreign import ccall unsafe "NR_Client_SetFont" c_clientSetFont :: Client -> CString -> IO ()

-- Set the font size.
setFontSize :: Client -> Int -> Int -> IO ()
setFontSize client w h = c_clientSetFontSize client (fromIntegral w) (fromIntegral h)
foreign import ccall unsafe "NR_Client_SetFontSize" c_clientSetFontSize :: Client -> CInt -> CInt -> IO ()

-- Handle events...
-- TODO

-- Set the size of the window.
setSize :: Client -> Int -> Int -> IO ()
setSize client w h = c_clientSetSize client (fromIntegral w) (fromIntegral h)
foreign import ccall unsafe "NR_Client_SetSize" c_clientSetSize :: Client -> CInt -> CInt -> IO ()

-- Get the size of the window
getSize :: Client -> IO (Int, Int)
getSize client = do
  w <- malloc :: IO (Ptr CInt)
  h <- malloc :: IO (Ptr CInt)
  c_clientGetSize client w h
  rw <- peek w
  rh <- peek h
  free w
  free h
  return (fromIntegral rw, fromIntegral rh)
foreign import ccall unsafe "NR_Client_GetSize" c_clientGetSize :: Client -> Ptr CInt -> Ptr CInt -> IO ()

-- Set the caption
setCaption :: Client -> String -> IO ()
setCaption client c = withCString c $ \cc -> c_clientSetCaption client cc
foreign import ccall unsafe "NR_Client_SetCaption" c_clientSetCaption :: Client -> CString -> IO ()

-- Get the caption
-- TODO

-- Set a glyph
-- TODO

-- Draw text
text :: Client -> Int -> Int -> String -> IO ()
text client x y t = withCString t $ \ct -> c_clientText client (fromIntegral x) (fromIntegral y) ct
foreign import ccall unsafe "NR_Client_Text" c_clientText :: Client -> CInt -> CInt -> CString -> IO ()
