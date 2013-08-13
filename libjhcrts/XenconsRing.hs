module XenconsRing(xenconsRingInit) where
import Foreign.Ptr
import Data.Word
import Foreign.C.String
import XenconsoleStub
import Xen
import Struct
import Util

foreign import ccall "console/xencons_ring.c hs_get_evtch" getEvtch :: IO Word32
foreign import ccall "console/xencons_ring.c hs_get_mfn" getMfn :: IO Word64
foreign import ccall "console/xencons_ring.c hs_mfn_to_virt" mfnToVirt :: Word64 -> IO (Ptr Word8)
foreign import ccall "console/xencons_ring.c hs_notify_daemon" notifyDaemon :: Ptr ConsfrontDev -> IO ()
foreign import ccall "console/console.c xencons_tx" xenconsTx :: IO ()
foreign import ccall "console/console.c xencons_rx" xenconsRx :: CString -> Word32 -> Ptr Word8 -> IO ()
foreign import ccall "hs_mb" mb :: IO ()
foreign import capi  "MASK_XENCONS_IDX" maskXenconsIdx :: Word32 -> Ptr a -> Word32

foreign export ccall "hs_xencons_ring_init" xenconsRingInit :: IO (Ptr Word8)
foreign export ccall "hs__handle_input" handleInput :: EvtchnPort -> Ptr Word8 -> Ptr Word8 -> IO ()
foreign import unsafe ccall "&hs__handle_input" ptrHandleInput :: FunPtr (IO())

xenconsRingInit = do evtch <- getEvtch
                     if evtch == 0 then
                       return nullPtr
                     else
                       do nodename <- newCString "device/console"
                          ring     <- mfnToVirt =<< getMfn
                          let dev = ConsfrontDev {
                                      consfrontDevNodename = nodename
                                      , consfrontDevDom     = 0
                                      , consfrontDevBackend = nullPtr
                                      , consfrontDevRingRef = 0
                                      , consfrontDevEvtchn  = evtch
                                      , consfrontDevEvents  = nullPtr
                                      , consfrontDevRing    = castPtr ring
                                    }
                          ptrDev <- toC dev
--                          ptrHandleInput <- handleInput
                          err <-  bindEvtchn evtch ptrHandleInput $ castPtr ptrDev
                          if err <= 0 then
                            do printk $ "XEN console request chn bind failed " ++ show err ++ "\n"
                               return nullPtr
                          else
                            do unmaskEvtch evtch
                               notifyDaemon ptrDev
                               return nullPtr

xenconsInterface :: IO (Ptr XenconsInterface)
xenconsInterface = castPtr `fmap` (mfnToVirt =<< getMfn)

handleInput port regs arg =
  do let dev = castPtr arg
     ptrIntf <- xenconsInterface
     cons <- getXenconsInterfaceInCons ptrIntf
     prod <- getXenconsInterfaceInProd ptrIntf
     mb
     loop cons prod ptrIntf
     mb
     setXenconsInterfaceInCons ptrIntf prod
     notifyDaemon dev
     xenconsTx
     return ()
  where loop cons prod ptrIntf =
          if cons == prod then
            return ()
          else
            do in_ <- getXenconsInterfaceIn_ ptrIntf
               let idx = fromInteger $ toInteger $ maskXenconsIdx cons in_
               xenconsRx (in_ `plusPtr` idx) 1 regs
               loop (cons+1) prod ptrIntf
