module Xen where
import Data.Word
import Foreign.Ptr

foreign import ccall "hypervisor.h unmask_evtchn" unmaskEvtch :: Word32 -> IO ()
foreign import ccall "events.h bind_virq" bindVirq :: Word32 -> FunPtr a -> Ptr Word8 -> IO Word32

virqTimer :: Word32
virqTimer = 0
