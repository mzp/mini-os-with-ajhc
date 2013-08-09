module Struct where
import Foreign.Ptr
class Struct a where
  fromC :: Ptr a -> IO a
  toC :: a -> IO (Ptr a)
