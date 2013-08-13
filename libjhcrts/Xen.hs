module Xen where
import Data.Word
import Foreign.Ptr

foreign import ccall "hypervisor.h unmask_evtchn" unmaskEvtch :: Word32 -> IO ()

type EvtchnPort = Word32
foreign import ccall "events.h bind_virq" bindVirq :: EvtchnPort -> FunPtr a -> Ptr Word8 -> IO EvtchnPort
foreign import ccall "events.h bind_evtchn" bindEvtchn :: EvtchnPort -> FunPtr a -> Ptr Word8 -> IO EvtchnPort

virqTimer :: Word32
virqTimer = 0
