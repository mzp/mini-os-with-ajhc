module Xenbus where
import Foreign.Ptr
import Foreign.C.String
import Data.List
import Data.Maybe
import Control.Monad
import Sched
import Xen
import Util
import XsWireStub
import XenbusStub

-- stub function
newtype WaitQueueHead = WaitQueueHead Word8
foreign import ccall "string.h strlen" strlen :: CString -> IO Int
foreign import ccall "wait.h wake_up" wakeUp :: Ptr WaitQueueHead -> IO ()
foreign import primitive "const.sizeof(struct xsd_sockmsg)" msgSize :: Word32
foreign import primitive "const.sizeof(struct xenbus_event)" eventSize :: Int
foreign import primitive "const.XS_WATCH_EVENT" xsWatchEvent :: Word32
foreign import capi "MASK_XENSTORE_IDX" maskXenstoreMask :: Word32 -> IO Word32
foreign import ccall "xen/io/xs_wire.h hs_get_store_mfn" getStoreMfn :: IO Word64
foreign import ccall "memcpy_from_ring" memcpyFromRing :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> IO ()
foreign import ccall "xenbus.h xenbus_read_integer" xenbusReadInteger :: CString -> IO Int

foreign import ccall "hs_set_xenstore_buf" setXenStoreBuf :: Ptr XenstoreDomainInterface -> IO ()
foreign import ccall "hs_get_xenstore_buf" getXenStoreBuf :: IO (Ptr XenstoreDomainInterface)
foreign import ccall "hs_get_store_evtchn" getStoreEvtchn :: IO EvtchnPort
foreign import ccall "hs_xenbus_evtchn_handler" xenbusEvtchnHandler :: IO (FunPtr (IO()))
foreign import ccall "hs_wait_event" waitEvent :: IO ()
foreign import ccall "hs_make_event" makeEvent :: Int -> IO (Ptr XenbusEvent)
foreign import ccall "hs_make_msg" makeMsg :: Word32 -> IO (Ptr Word8)
foreign import ccall "hs_get_watches" rawGetWatches :: IO (Ptr Watch)
foreign import ccall "hs_add_head" addHead :: Ptr XenbusEvent -> Ptr (Ptr XenbusEvent) -> IO ()
foreign import ccall "hs_get_watch_queue" getWatchQueue :: IO (Ptr WaitQueueHead)
foreign import ccall "hs_get_req_info" getReqInfo :: Word32 -> IO (Ptr XenbusReqInfo)
foreign import ccall "hs_get_waitq" getWaitq :: Word32 -> IO (Ptr WaitQueueHead)

foreign export ccall "init_xenbus" initXenbus :: IO ()
foreign export ccall "hs_xenbus_thread" xenbusThread :: Ptr Word8 -> IO ()
foreign import unsafe ccall "&hs_xenbus_thread" xenbusThreadFunc :: FunPtr (IO())

initXenbus :: IO ()
initXenbus = do printk "init_xenbus called!\n"
                storeMfn <- getStoreMfn
                xenStoreBuf <- mfnToVirt storeMfn
                setXenStoreBuf $ castPtr xenStoreBuf
                printk $ "buf at " ++ show xenStoreBuf ++ "\n"
                threadName <- newCString "xenstore"
                createThread threadName xenbusThreadFunc nullPtr
                eventPort <- getStoreEvtchn
                handler <- xenbusEvtchnHandler
                err <- bindEvtchn eventPort handler nullPtr
                unmaskEvtch eventPort
                printk $ "xenbus initialised on irq " ++ show err ++ "mfn " ++ show storeMfn ++ "\n"
                return ()

xenbusThread _ = do  msg <- mkXsdSockmsg
                     forever $ do waitEvent
                                  thread msg

toList :: Ptr Watch -> IO [Ptr Watch]
toList w = if w == nullPtr then
                return []
              else
                do ws  <- getWatchNext w
                   ws' <- toList ws
                   return (w : ws')

watchList :: IO [Ptr Watch]
watchList = rawGetWatches >>= toList

foreign import ccall "abort" abort :: IO ()

thread msg = do buf <- getXenStoreBuf
                prod <- getXenstoreDomainInterfaceRspProd buf
                cons <- getXenstoreDomainInterfaceRspCons buf
                if prod - cons < msgSize then
                  return ()
                else
                  do rmb
                     rsp <- getXenstoreDomainInterfaceRsp buf
                     offset <- maskXenstoreMask =<< getXenstoreDomainInterfaceRspCons buf
                     memcpyFromRing (castPtr rsp) (castPtr msg) (fromInteger $ toInteger offset) (fromInteger $ toInteger msgSize)
                     len <- getXsdSockmsgLen msg

                     if prod - cons < msgSize + len then
                       return ()
                     else
                       do t <- getXsdSockmsgType_ msg
                          if t == xsWatchEvent then
                            do event <- makeEvent $ fromInteger $ toInteger len
                               let d = castPtr $ event `plusPtr` eventSize
                               offset <- maskXenstoreMask (cons + msgSize)
                               memcpyFromRing (castPtr rsp) (castPtr d) (fromInteger $ toInteger offset) (fromInteger $ toInteger len)
                               setXenbusEventPath event d
                               pathLen <- strlen d
                               setXenbusEventToken event $ d `plusPtr` pathLen `plusPtr` 1
                               setXenstoreDomainInterfaceRspCons buf (cons + len + msgSize)
                               token <- peekCString =<< getXenbusEventToken event
                               events <- liftM listToMaybe $ mapM getWatchEvents =<< filterM (eqToken token) =<< watchList
                               case events of
                                 Just e -> do addHead event e
                                              wakeUp =<< getWatchQueue
                                              return ()
                                 Nothing -> return ()
                          else
                            do reqInfo <- getReqInfo =<< getXsdSockmsgReqId msg
                               reply   <- makeMsg len
                               setXenbusReqInfoReply reqInfo reply
                               offset <- maskXenstoreMask cons
                               memcpyFromRing (castPtr rsp) reply (fromInteger $ toInteger offset) (fromInteger $ toInteger $ len + msgSize)
                               setXenstoreDomainInterfaceRspCons buf $ cons + len + msgSize
                               wakeUp =<< getWaitq  =<< getXsdSockmsgReqId msg
                          thread msg
  where eqToken token w = do t <- peekCString =<< getWatchToken w
                             return $ token == t

