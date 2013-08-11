module XenconsRing(xenconsRingInit) where
import Foreign.Ptr
import Data.Word
foreign import ccall "console/xencons_ring.c xencons_ring_init" xenconsRingInit :: IO (Ptr Word8)

