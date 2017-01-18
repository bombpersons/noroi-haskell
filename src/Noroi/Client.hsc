{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Noroi.Client (
  newClient,
  deleteClient,
  setFont,
  setFontSize,

  handleEvent,

  setSize,
  getSize,

  setCaption,

  text,

  swapBuffers,

  MouseButton(..),
  Event(..),
  Client(..)
) where

import Noroi.Context

import Foreign
import Foreign.C.String
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc

#include <noroi/base/noroi_types.h>
#include <noroi/client/noroi_client.h>

data MouseButton = MouseButtonLeft | MouseButtonMiddle | MouseButtonRight
                  deriving (Show, Eq)
convMouseButton :: CInt -> MouseButton
convMouseButton i = case i of
                      0 -> MouseButtonLeft
                      1 -> MouseButtonMiddle
                      2 -> MouseButtonRight
convToInt :: MouseButton -> CInt
convToInt button = case button of
                      MouseButtonLeft -> 0
                      MouseButtonMiddle -> 1
                      MouseButtonRight -> 2

data Event = MousePressEvent MouseButton |
             MouseReleaseEvent MouseButton |
             MouseMoveEvent Int Int |
             MouseScrollEvent Int |
             --KeyEventPress Key KeyMod |
             --KeyEventRelease Key KeyMod |
             ResizeEvent Int Int |
             QuitEvent |
             UndefinedEvent
             deriving (Show, Eq)

instance Storable Event where
  sizeOf _ = #{size NR_Event}
  alignment _ = alignment (undefined :: CInt)
  peek p = do
    t <- peekByteOff p 0 :: IO CInt
    e <- case t of
      0 -> do
        button <- peekByteOff p 4 :: IO CInt
        return $ MousePressEvent $ convMouseButton button
      1 -> do
        button <- peekByteOff p 4 :: IO CInt
        return $ MousePressEvent $ convMouseButton button

      2 -> do
        x <- peekByteOff p 4 :: IO CInt
        y <- peekByteOff p 8 :: IO CInt
        return $ MouseMoveEvent (fromIntegral x) (fromIntegral y)
      3 -> do
        scroll <- peekByteOff p 4 :: IO CInt
        return $ MouseScrollEvent (fromIntegral scroll)
      --4 -> KeyEventPress (`fmap` peekByteOff p 4) (`fmap` peekByteOff p 8)
      --5 -> KeyEventRelease (`fmap` peekByteOff p 4) (`fmap` peekByteOff p 8)
      6 -> do
        width <- peekByteOff p 4 :: IO CInt
        height <- peekByteOff p 8 :: IO CInt
        return $ ResizeEvent (fromIntegral width) (fromIntegral height)
      7 -> return QuitEvent
      _ -> return UndefinedEvent
    return e

  poke p (MousePressEvent button) = pokeByteOff p 0 (CInt 0) >> pokeByteOff p 4 (convToInt button)
  poke p (MouseReleaseEvent button) = pokeByteOff p 0 (CInt 1) >> pokeByteOff p 4 (convToInt button)
  poke p (MouseMoveEvent x y) = pokeByteOff p 0 (CInt 2) >> pokeByteOff p 4 x >> pokeByteOff p 8 y
  poke p (MouseScrollEvent scroll) = pokeByteOff p 0 (CInt 3) >> pokeByteOff p 4 scroll
  --poke p (KeyEventPress key keyMod) = pokeByteOff p 0 4 >> pokeByteOff p 4 key >> pokeByteOff p 8 keyMod
  --poke p (KeyEventRelease key keyMod) = pokeByteOff p 0 5 >> pokeByteOff p 4 key >> pokeByteOff p 8 keyMod
  poke p (ResizeEvent x y) = pokeByteOff p 0 (CInt 6) >> pokeByteOff p 4 x >> pokeByteOff p 8 y
  poke p (QuitEvent) = pokeByteOff p 0 (CInt 7)

newtype Client = Client (Ptr Client)

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
handleEvent :: Client -> IO (Maybe (Event))
handleEvent client =  do
  event <- alloca $ \p -> do
      suc <- c_clientHandleEvent client p
      if suc <= 0 then return $ Nothing
      else do
        u <- peek p
        return $ Just u
  return event

foreign import ccall unsafe "NR_Client_HandleEvent" c_clientHandleEvent :: Client -> Ptr Event -> IO (CInt)

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

-- Swap buffers
swapBuffers :: Client -> IO ()
swapBuffers client = c_clientSwapBuffers client
foreign import ccall unsafe "NR_Client_SwapBuffers" c_clientSwapBuffers :: Client -> IO ()
