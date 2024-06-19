module Lib
  ( add,
  )
where

import Foreign.C.Types

foreign import ccall "add"
  rust_add :: CLong -> CLong -> CLong

add :: Int -> Int -> Int
add a b = fromIntegral $ rust_add (fromIntegral a) (fromIntegral b)
