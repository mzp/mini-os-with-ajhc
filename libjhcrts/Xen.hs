module Xen where
import Data.Word
import Foreign.Ptr

foreign import ccall "hs_rmb" rmb :: IO ()
foreign import ccall "hs_mfn_to_virt" mfnToVirt :: Word64 -> IO (Ptr Word8)

foreign import ccall "hypervisor.h unmask_evtchn" unmaskEvtch :: Word32 -> IO ()
foreign import ccall "hypervisor.h force_evtchn_callback" forceEvtchnCallback :: IO ()

type EvtchnPort = Word32
foreign import ccall "events.h bind_virq" bindVirq :: EvtchnPort -> FunPtr a -> Ptr Word8 -> IO EvtchnPort
foreign import ccall "events.h bind_evtchn" bindEvtchn :: EvtchnPort -> FunPtr a -> Ptr Word8 -> IO EvtchnPort
foreign import ccall "evtchn_alloc_unbound" evtchnAllocUnbound ::  Word16 ->  FunPtr (IO ()) -> Ptr Word8 -> Ptr EvtchnPort -> IO Int

virqTimer :: Word32
virqTimer = 0

foreign import primitive "const.DOMID_SELF" domidSelf :: Word16
foreign import primitive "const.PAGE_SIZE" pageSize :: Word32
foreign import primitive "const.GNTTABOP_setup_table" opSetupTable :: Word32
foreign import primitive "const.sizeof(grant_entry_t)" entrySize :: Word32

foreign import ccall "mini-os/gnttab.h HYPERVISOR_grant_table_op" hypervisorGrantTableOp :: Word32 -> Ptr a -> Word32 -> IO ()
foreign import ccall "mini-os/gnttab.h map_frames" mapFrames :: Ptr Word64 -> Word64 -> IO (Ptr a)
foreign import capi  "mini-os/semaphore.h up" up :: Ptr a -> IO ()

type Time = Int64
foreign import capi  "mini-os/time.h NOW" getNow :: IO Time
foreign import capi  "mini-os/time.h SECONDS" seconds :: Int -> Time
foreign import ccall "mini-os/time.h block_domain" blockDomain :: Time -> IO ()

foreign import ccall "hs_local_irq_save" localIrqSave :: Word32 -> IO Word32
foreign import ccall "hs_local_irq_restore" localIrqRestore :: Word32 -> IO Word32


foreign import primitive "const.STACK_SIZE_PAGE_ORDER " stackSizePageOrder :: Int
foreign import ccall "mini-os/mm.h alloc_pages" allocPages :: Int -> IO (Ptr a)
allocPage = allocPages 0
foreign import ccall "mini-os/mm.h free_pages" freePages :: Ptr a -> Int -> IO ()
foreign import capi  "mini-os/mm.h virt_to_mfn" virtToMfn :: Ptr a -> Ptr a
foreign import ccall "mini-os/xmalloc.h xfree" xfree :: Ptr a -> IO ()

foreign import ccall "hs_bug" bug :: IO ()
