module Time where
import Data.Word
import Data.Bits
import Foreign.Ptr
import Xen
import Util
import TimeShadowInfoStub
import Struct

--------------------------
-- custom import function
--------------------------
foreign import ccall "hs_get_shadow" getShadow :: IO (Ptr ShadowTimeInfo)
foreign import ccall "hs_get_shadow_ts_version" getShadowTsVersion :: IO Word32
foreign import ccall "hs_set_shadow_ts_version" setShadowTsVersion :: Word32 -> IO ()

foreign import ccall "hs_get_shared_info" getSharedInfo :: IO (Ptr SharedInfo)
foreign import ccall "hs_get_vcpu_time_info" getVCPUTimeInfo :: IO (Ptr VcpuTimeInfo)
foreign import ccall "hs_get_shadow_ts" getShadowTs :: IO (Ptr Timespec)

foreign import ccall "hs_set_port" setPort :: Word32 -> IO ()

--------------------------
-- export Haskell function
--------------------------
updateWallclock :: IO ()

foreign export ccall "get_time_values_from_xen" getTimeValuesFromXen :: IO ()
getTimeValuesFromXen :: IO ()

foreign export ccall "timer_handler" timerHandler :: Word32 -> Ptr Word8 -> Ptr Word8 -> IO ()
timerHandler :: Word32 -> Ptr Word8 -> Ptr Word8 -> IO ()

foreign export ccall "init_time" initTime :: IO ()
initTime :: IO ()

foreign import unsafe ccall "&timer_handler" ptrTimerHandler :: FunPtr (IO())
---
updateWallclock = do shared <- getSharedInfo
                     v <- getSharedInfoWcVersion shared
                     sv <- getShadowTsVersion
                     main
                     let b = (v .&. 1) .|. (sv `xor` v)
                     if b == 0 then
                       return ()
                     else
                       main >> updateWallclock
  where main = do shared <- getSharedInfo
                  getSharedInfoWcVersion shared >>= setShadowTsVersion
                  rmb
                  shadow_ts <- getShadowTs
                  getSharedInfoWcSec shared  >>= (setTimespecTvSec shadow_ts).fromInteger.toInteger
                  getSharedInfoWcNsec shared >>= (setTimespecTvNsec shadow_ts).fromInteger.toInteger
                  rmb

getTimeValuesFromXen = do info <- getVCPUTimeInfo
                          version <- getVcpuTimeInfoVersion info
                          shadow  <- getShadow
                          sv      <- getShadowTimeInfoVersion shadow
                          let b = (version .&. 1) .|. (sv `xor` version)
                          main
                          if b == 0 then
                            do n <- getVcpuTimeInfoTscToSystemMul info
                               setShadowTimeInfoTscToUsecMul shadow (n `div` 1000)
                          else
                            main >> getTimeValuesFromXen
  where main = do info <- getVCPUTimeInfo
                  shadow <- getShadow
                  getVcpuTimeInfoVersion info >>= setShadowTimeInfoVersion shadow
                  rmb
                  getVcpuTimeInfoTscTimestamp info >>= setShadowTimeInfoTscTimestamp shadow
                  getVcpuTimeInfoSystemTime info >>= setShadowTimeInfoSystemTimestamp shadow
                  getVcpuTimeInfoTscToSystemMul info >>= setShadowTimeInfoTscToNsecMul shadow
                  getVcpuTimeInfoTscShift info >>= setShadowTimeInfoTscShift shadow . fromInteger . toInteger
                  rmb


timerHandler _ _ _ = do getTimeValuesFromXen
                        updateWallclock

initTime = do printk "Initialising timer interface\n"
              let p = ptrTimerHandler
              port <- bindVirq virqTimer p nullPtr
              setPort port
              unmaskEvtch port
