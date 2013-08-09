module TimeShadowInfoStub where
import Foreign.Ptr
import Struct


data ShadowTimeInfo = ShadowTimeInfo {
  
    
    shadowTimeInfoTscTimestamp :: Word64
  
     , 
    shadowTimeInfoSystemTimestamp :: Word64
  
     , 
    shadowTimeInfoTscToNsecMul :: Word32
  
     , 
    shadowTimeInfoTscToUsecMul :: Word32
  
     , 
    shadowTimeInfoTscShift :: Int32
  
     , 
    shadowTimeInfoVersion :: Word32
  
  }

data VcpuTimeInfo = VcpuTimeInfo {
  
    
    vcpuTimeInfoVersion :: Word32
  
     , 
    vcpuTimeInfoTscTimestamp :: Word64
  
     , 
    vcpuTimeInfoSystemTime :: Word64
  
     , 
    vcpuTimeInfoTscToSystemMul :: Word32
  
     , 
    vcpuTimeInfoTscShift :: Int8
  
  }

data SharedInfo = SharedInfo {
  
    
    sharedInfoWcVersion :: Word32
  
     , 
    sharedInfoWcSec :: Word32
  
     , 
    sharedInfoWcNsec :: Word32
  
  }

data Timespec = Timespec {
  
    
    timespecTvSec :: Int64
  
     , 
    timespecTvNsec :: Int32
  
  }



foreign import ccall "hs_new_shadow_time_info" mkShadowTimeInfo :: IO (Ptr ShadowTimeInfo)


foreign import ccall "hs_set_shadow_time_info_tsc_timestamp" setShadowTimeInfoTscTimestamp :: Ptr ShadowTimeInfo -> Word64 -> IO ()

  
foreign import ccall "hs_get_shadow_time_info_tsc_timestamp" getShadowTimeInfoTscTimestamp :: Ptr ShadowTimeInfo -> IO Word64
  

foreign import ccall "hs_set_shadow_time_info_system_timestamp" setShadowTimeInfoSystemTimestamp :: Ptr ShadowTimeInfo -> Word64 -> IO ()

  
foreign import ccall "hs_get_shadow_time_info_system_timestamp" getShadowTimeInfoSystemTimestamp :: Ptr ShadowTimeInfo -> IO Word64
  

foreign import ccall "hs_set_shadow_time_info_tsc_to_nsec_mul" setShadowTimeInfoTscToNsecMul :: Ptr ShadowTimeInfo -> Word32 -> IO ()

  
foreign import ccall "hs_get_shadow_time_info_tsc_to_nsec_mul" getShadowTimeInfoTscToNsecMul :: Ptr ShadowTimeInfo -> IO Word32
  

foreign import ccall "hs_set_shadow_time_info_tsc_to_usec_mul" setShadowTimeInfoTscToUsecMul :: Ptr ShadowTimeInfo -> Word32 -> IO ()

  
foreign import ccall "hs_get_shadow_time_info_tsc_to_usec_mul" getShadowTimeInfoTscToUsecMul :: Ptr ShadowTimeInfo -> IO Word32
  

foreign import ccall "hs_set_shadow_time_info_tsc_shift" setShadowTimeInfoTscShift :: Ptr ShadowTimeInfo -> Int32 -> IO ()

  
foreign import ccall "hs_get_shadow_time_info_tsc_shift" getShadowTimeInfoTscShift :: Ptr ShadowTimeInfo -> IO Int32
  

foreign import ccall "hs_set_shadow_time_info_version" setShadowTimeInfoVersion :: Ptr ShadowTimeInfo -> Word32 -> IO ()

  
foreign import ccall "hs_get_shadow_time_info_version" getShadowTimeInfoVersion :: Ptr ShadowTimeInfo -> IO Word32
  


foreign import ccall "hs_new_vcpu_time_info" mkVcpuTimeInfo :: IO (Ptr VcpuTimeInfo)


foreign import ccall "hs_set_vcpu_time_info_version" setVcpuTimeInfoVersion :: Ptr VcpuTimeInfo -> Word32 -> IO ()

  
foreign import ccall "hs_get_vcpu_time_info_version" getVcpuTimeInfoVersion :: Ptr VcpuTimeInfo -> IO Word32
  

foreign import ccall "hs_set_vcpu_time_info_tsc_timestamp" setVcpuTimeInfoTscTimestamp :: Ptr VcpuTimeInfo -> Word64 -> IO ()

  
foreign import ccall "hs_get_vcpu_time_info_tsc_timestamp" getVcpuTimeInfoTscTimestamp :: Ptr VcpuTimeInfo -> IO Word64
  

foreign import ccall "hs_set_vcpu_time_info_system_time" setVcpuTimeInfoSystemTime :: Ptr VcpuTimeInfo -> Word64 -> IO ()

  
foreign import ccall "hs_get_vcpu_time_info_system_time" getVcpuTimeInfoSystemTime :: Ptr VcpuTimeInfo -> IO Word64
  

foreign import ccall "hs_set_vcpu_time_info_tsc_to_system_mul" setVcpuTimeInfoTscToSystemMul :: Ptr VcpuTimeInfo -> Word32 -> IO ()

  
foreign import ccall "hs_get_vcpu_time_info_tsc_to_system_mul" getVcpuTimeInfoTscToSystemMul :: Ptr VcpuTimeInfo -> IO Word32
  

foreign import ccall "hs_set_vcpu_time_info_tsc_shift" setVcpuTimeInfoTscShift :: Ptr VcpuTimeInfo -> Int8 -> IO ()

  
foreign import ccall "hs_get_vcpu_time_info_tsc_shift" getVcpuTimeInfoTscShift :: Ptr VcpuTimeInfo -> IO Int8
  


foreign import ccall "hs_new_shared_info" mkSharedInfo :: IO (Ptr SharedInfo)


foreign import ccall "hs_set_shared_info_wc_version" setSharedInfoWcVersion :: Ptr SharedInfo -> Word32 -> IO ()

  
foreign import ccall "hs_get_shared_info_wc_version" getSharedInfoWcVersion :: Ptr SharedInfo -> IO Word32
  

foreign import ccall "hs_set_shared_info_wc_sec" setSharedInfoWcSec :: Ptr SharedInfo -> Word32 -> IO ()

  
foreign import ccall "hs_get_shared_info_wc_sec" getSharedInfoWcSec :: Ptr SharedInfo -> IO Word32
  

foreign import ccall "hs_set_shared_info_wc_nsec" setSharedInfoWcNsec :: Ptr SharedInfo -> Word32 -> IO ()

  
foreign import ccall "hs_get_shared_info_wc_nsec" getSharedInfoWcNsec :: Ptr SharedInfo -> IO Word32
  


foreign import ccall "hs_new_timespec" mkTimespec :: IO (Ptr Timespec)


foreign import ccall "hs_set_timespec_tv_sec" setTimespecTvSec :: Ptr Timespec -> Int64 -> IO ()

  
foreign import ccall "hs_get_timespec_tv_sec" getTimespecTvSec :: Ptr Timespec -> IO Int64
  

foreign import ccall "hs_set_timespec_tv_nsec" setTimespecTvNsec :: Ptr Timespec -> Int32 -> IO ()

  
foreign import ccall "hs_get_timespec_tv_nsec" getTimespecTvNsec :: Ptr Timespec -> IO Int32
  



-- type class

instance Struct ShadowTimeInfo where
  fromC ptr = do {
    
      
        tscTimestamp <- getShadowTimeInfoTscTimestamp ptr;
      
    
      
        systemTimestamp <- getShadowTimeInfoSystemTimestamp ptr;
      
    
      
        tscToNsecMul <- getShadowTimeInfoTscToNsecMul ptr;
      
    
      
        tscToUsecMul <- getShadowTimeInfoTscToUsecMul ptr;
      
    
      
        tscShift <- getShadowTimeInfoTscShift ptr;
      
    
      
        version <- getShadowTimeInfoVersion ptr;
      
    
    return ShadowTimeInfo
    
      {
    
      
        
        shadowTimeInfoTscTimestamp = tscTimestamp
      
    
      
         , 
        shadowTimeInfoSystemTimestamp = systemTimestamp
      
    
      
         , 
        shadowTimeInfoTscToNsecMul = tscToNsecMul
      
    
      
         , 
        shadowTimeInfoTscToUsecMul = tscToUsecMul
      
    
      
         , 
        shadowTimeInfoTscShift = tscShift
      
    
      
         , 
        shadowTimeInfoVersion = version
      
    
    }
    
  }

  toC x = do {
    ptr <- mkShadowTimeInfo;
    
      setShadowTimeInfoTscTimestamp ptr (shadowTimeInfoTscTimestamp x);
    
      setShadowTimeInfoSystemTimestamp ptr (shadowTimeInfoSystemTimestamp x);
    
      setShadowTimeInfoTscToNsecMul ptr (shadowTimeInfoTscToNsecMul x);
    
      setShadowTimeInfoTscToUsecMul ptr (shadowTimeInfoTscToUsecMul x);
    
      setShadowTimeInfoTscShift ptr (shadowTimeInfoTscShift x);
    
      setShadowTimeInfoVersion ptr (shadowTimeInfoVersion x);
    
    return ptr
  }

instance Struct VcpuTimeInfo where
  fromC ptr = do {
    
      
        version <- getVcpuTimeInfoVersion ptr;
      
    
      
        tscTimestamp <- getVcpuTimeInfoTscTimestamp ptr;
      
    
      
        systemTime <- getVcpuTimeInfoSystemTime ptr;
      
    
      
        tscToSystemMul <- getVcpuTimeInfoTscToSystemMul ptr;
      
    
      
        tscShift <- getVcpuTimeInfoTscShift ptr;
      
    
    return VcpuTimeInfo
    
      {
    
      
        
        vcpuTimeInfoVersion = version
      
    
      
         , 
        vcpuTimeInfoTscTimestamp = tscTimestamp
      
    
      
         , 
        vcpuTimeInfoSystemTime = systemTime
      
    
      
         , 
        vcpuTimeInfoTscToSystemMul = tscToSystemMul
      
    
      
         , 
        vcpuTimeInfoTscShift = tscShift
      
    
    }
    
  }

  toC x = do {
    ptr <- mkVcpuTimeInfo;
    
      setVcpuTimeInfoVersion ptr (vcpuTimeInfoVersion x);
    
      setVcpuTimeInfoTscTimestamp ptr (vcpuTimeInfoTscTimestamp x);
    
      setVcpuTimeInfoSystemTime ptr (vcpuTimeInfoSystemTime x);
    
      setVcpuTimeInfoTscToSystemMul ptr (vcpuTimeInfoTscToSystemMul x);
    
      setVcpuTimeInfoTscShift ptr (vcpuTimeInfoTscShift x);
    
    return ptr
  }

instance Struct SharedInfo where
  fromC ptr = do {
    
      
        wcVersion <- getSharedInfoWcVersion ptr;
      
    
      
        wcSec <- getSharedInfoWcSec ptr;
      
    
      
        wcNsec <- getSharedInfoWcNsec ptr;
      
    
    return SharedInfo
    
      {
    
      
        
        sharedInfoWcVersion = wcVersion
      
    
      
         , 
        sharedInfoWcSec = wcSec
      
    
      
         , 
        sharedInfoWcNsec = wcNsec
      
    
    }
    
  }

  toC x = do {
    ptr <- mkSharedInfo;
    
      setSharedInfoWcVersion ptr (sharedInfoWcVersion x);
    
      setSharedInfoWcSec ptr (sharedInfoWcSec x);
    
      setSharedInfoWcNsec ptr (sharedInfoWcNsec x);
    
    return ptr
  }

instance Struct Timespec where
  fromC ptr = do {
    
      
        tvSec <- getTimespecTvSec ptr;
      
    
      
        tvNsec <- getTimespecTvNsec ptr;
      
    
    return Timespec
    
      {
    
      
        
        timespecTvSec = tvSec
      
    
      
         , 
        timespecTvNsec = tvNsec
      
    
    }
    
  }

  toC x = do {
    ptr <- mkTimespec;
    
      setTimespecTvSec ptr (timespecTvSec x);
    
      setTimespecTvNsec ptr (timespecTvNsec x);
    
    return ptr
  }

