module Sched(initSched) where
import Data.Bits
import Foreign.C.String
import Foreign.Ptr
import Xen
import Util
import List
import SchedStub

-- =====================================
-- Import function
-- =====================================
foreign import ccall "print_runqueue"
  dump :: IO ()
foreign import ccall "hs_dump"
  dumpThread :: Ptr Thread -> IO ()

foreign import ccall "mini-os/sched.h arch_create_thread"
  archCreateThread :: CString -> FunPtr (IO()) -> Ptr a -> IO (Ptr Thread)
foreign import ccall "mini-os/sched.h get_current"
  getCurrent :: IO (Ptr Thread)
foreign import ccall "hs_switch_threads"
  switchThreads :: Ptr Thread -> Ptr Thread -> IO ()
foreign import ccall "hs_get_thread_list"
  getThreadList :: Ptr Thread -> IO (Ptr CList)
foreign import ccall "hs_set_idle_thread"
  setIdleThread :: Ptr Thread -> IO ()
foreign import ccall "hs_get_idle_thread"
  getIdleThread :: IO (Ptr Thread)
foreign import ccall "hs_set_threads_started"
  setThreadsStarted :: Int -> IO ()
foreign import ccall "hs_get_in_callback"
  getInCallback :: IO Int
foreign import ccall "hs_get_entry"
  getEntry :: Ptr CList -> IO (Ptr Thread)
foreign import ccall "hs_exited_threads"
  getExitedThreads :: IO (Ptr CList)
foreign import ccall "mini-os/sched.h wake"
  wake :: Ptr Thread -> IO ()

-- =====================================
-- Constants
-- =====================================
foreign import primitive "const.RUNNABLE_FLAG"
  runnableFlag :: Word32

-- =====================================
-- Exported function
-- =====================================
foreign export "hs_idle_thread" idleThread :: Ptr Word8 -> IO ()
foreign import unsafe ccall "&hs_idle_thread" idleThreadFn :: FunPtr (IO ())
foreign export "init_sched" initSched :: IO ()
foreign export "schedule"   schedule :: IO ()
foreign export "hs_block" block :: Ptr Thread -> IO ()
foreign export "hs_create_thread" createThread :: CString -> FunPtr (IO ()) -> Ptr Word8 -> IO (Ptr Thread)

-- =====================================
-- Definition
-- =====================================
-- runnable flag
setRunnable :: Ptr Thread -> IO ()
clearRunnable :: Ptr Thread -> IO ()
isRunnable :: Ptr Thread -> IO Bool

setRunnable thread = do flags <- getThreadFlags thread
                        setThreadFlags thread (flags .|. runnableFlag)

clearRunnable thread = do flags <- getThreadFlags thread
                          setThreadFlags thread (flags .&. (complement runnableFlag))

isRunnable thread = do flags <- getThreadFlags thread
                       return $ (flags .&. runnableFlag) /= 0

block thread = do setThreadWakeupTime thread 0
                  clearRunnable thread

schedule = do prev  <- getCurrent
              flags <- localIrqSave 0
              n <- getInCallback
              if n /= 0 then
                printk "Must not call schedule() from callback\n" >> bug
              else
                return ()
              if flags /= 0 then
                printk "Must not call schedule() with IRQs disabled\n" >> bug
              else
                return ()
              next <- getRunnableThread
--              dumpThread prev
--              dumpThread next
              localIrqRestore flags
              if prev /= next then
                switchThreads prev next
              else
                return ()
              exitThreads <- mapM getEntry =<< toList =<< getExitedThreads
              mapM_ freeThread $ filter (/=prev) exitThreads
              return ()
  where
    getRunnableThread = do now <- getNow
                           let minWakeupTime = now + seconds 10
                           idleThread <- getIdleThread
                           threads    <- toList =<< getThreadList idleThread
                           ret <- getRunnableThread' now minWakeupTime threads
                           case ret of
                             Left until -> do blockDomain until
                                              forceEvtchnCallback
                                              getRunnableThread
                             Right x -> return x

    getRunnableThread' _ minWakeupTime []     =  do
                                                    return $ Left minWakeupTime
    getRunnableThread' now minWakeupTime (x:xs) =
      do thread <- getEntry x
         b <- isRunnable thread
         if b then
           do ts <- getThreadList thread
              miniosListDel ts
              idle <- getThreadList =<< getIdleThread
              miniosListAddTail ts idle
              return $ Right thread
         else
           do wakeupTime <- getThreadWakeupTime thread
              if 0 < wakeupTime && wakeupTime <= now then
                wake thread
              else
                return ()
              let nextWakeupTime = if wakeupTime <= 0 then
                                     minWakeupTime
                                   else
                                     minWakeupTime `min` wakeupTime
              getRunnableThread' now nextWakeupTime xs

    freeThread thread = do miniosListDel =<< getThreadList thread
                           stack <- getThreadStack thread
                           freePages stack stackSizePageOrder
--                           xfree thread
                           return ()

idleThread _ = do setThreadsStarted 1
                  loop
  where loop = do block =<< getCurrent
                  schedule
                  loop

createThread name function arg =
  do -- Call architecture specific setup.
     thread <- archCreateThread name function arg
     -- Not runable, not exited, not sleeping
     setThreadFlags      thread 0
     setThreadWakeupTime thread 0
     setRunnable thread
     flags <- localIrqSave 0
     idle  <- getIdleThread
     if idle /= nullPtr then
       do threadList <- getThreadList thread
          idleList   <- getThreadList idle
          miniosListAddTail threadList idleList
     else if function == idleThreadFn then
       return ()
     else
       do printk "BUG: Not allowed to create thread before initialising scheduler.\n"
          bug
     localIrqRestore flags
     return thread

initSched :: IO ()
initSched = do printk "Initialising scheduler\n"
               thread <- withCString "Idle" $ \idle -> createThread idle idleThreadFn nullPtr
               setIdleThread thread
               miniosInitListHead =<< getThreadList thread
               return ()
