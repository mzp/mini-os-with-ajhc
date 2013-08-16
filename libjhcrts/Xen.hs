module Xen where
import Data.Word
import Foreign.Ptr

foreign import ccall "hypervisor.h unmask_evtchn" unmaskEvtch :: Word32 -> IO ()

type EvtchnPort = Word32
foreign import ccall "events.h bind_virq" bindVirq :: EvtchnPort -> FunPtr a -> Ptr Word8 -> IO EvtchnPort
foreign import ccall "events.h bind_evtchn" bindEvtchn :: EvtchnPort -> FunPtr a -> Ptr Word8 -> IO EvtchnPort

virqTimer :: Word32
virqTimer = 0

foreign import primitive "const.DOMID_SELF" domidSelf :: Word16
foreign import primitive "const.PAGE_SIZE" pageSize :: Word32
foreign import primitive "const.GNTTABOP_setup_table" opSetupTable :: Word32
foreign import primitive "const.sizeof(grant_entry_t)" entrySize :: Word32

foreign import ccall "mini-os/gnttab.h HYPERVISOR_grant_table_op" hypervisorGrantTableOp :: Word32 -> Ptr a -> Word32 -> IO ()
foreign import ccall "mini-os/gnttab.h map_frames" mapFrames :: Ptr Word64 -> Word64 -> IO (Ptr a)
foreign import capi  "mini-os/semaphore.h up" up :: Ptr a -> IO ()


