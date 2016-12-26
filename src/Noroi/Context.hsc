{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Noroi.Context (
  newContext,
  deleteContext,

  Context(..)
) where

import Foreign
import Foreign.Ptr
import Foreign.C.Types

newtype Context = Context (Ptr Context)

#include <noroi/base/noroi.h>

-- Create a new context.
newContext = c_contextNew
foreign import ccall unsafe "NR_Context_New" c_contextNew :: IO Context

-- Delete a context.
deleteContext = c_contextDelete
foreign import ccall unsafe "NR_Context_Delete" c_contextDelete :: Context -> IO ()
