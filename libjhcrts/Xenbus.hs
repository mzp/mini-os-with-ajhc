module Xenbus where
import Foreign.Ptr
import Foreign.C.String
import Sched
import Xen
import Util

foreign import ccall "hs_get_store_mfn" getStoreMfn :: IO Word64
foreign import ccall "hs_set_xenstore_buf" setXenStoreBuf :: Ptr Word8 -> IO ()
foreign import ccall "hs_get_store_evtchn" getStoreEvtchn :: IO EvtchnPort
foreign import ccall "hs_xenbus_thread_func" xenbusThreadFunc :: IO (FunPtr (IO()))
foreign import ccall "hs_xenbus_evtchn_handler" xenbusEvtchnHandler :: IO (FunPtr (IO()))
foreign export ccall "hs_init_xenbus" initXenbus :: IO ()

initXenbus :: IO ()
initXenbus = do printk "init_xenbus called!\n"
                storeMfn <- getStoreMfn
                xenStoreBuf <- mfnToVirt storeMfn
                setXenStoreBuf xenStoreBuf
                printk $ "buf at " ++ show xenStoreBuf ++ "\n"
                threadName <- newCString "xenstore"
                f <- xenbusThreadFunc
                createThread threadName f nullPtr
                eventPort <- getStoreEvtchn
                handler <- xenbusEvtchnHandler
                err <- bindEvtchn eventPort handler nullPtr
                unmaskEvtch eventPort
                printk $ "xenbus initialised on irq " ++ show err ++ "mfn " ++ show storeMfn ++ "\n"
                return ()
