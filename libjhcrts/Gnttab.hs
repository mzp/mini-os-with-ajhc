module Gnttab where
import Foreign.Marshal.Array
import Foreign.Ptr
import Data.Word
import Util
import GnttabStub
import Struct

type GrantRef= Int32
foreign import primitive "const.DOMID_SELF" domidSelf :: Word16
foreign import primitive "const.PAGE_SIZE" pageSize :: Word32
foreign import primitive "const.GNTTABOP_setup_table" opSetupTable :: Word32
foreign import primitive "const.sizeof(grant_entry_t)" entrySize :: Word32

foreign import ccall "mini-os/gnttab.h HYPERVISOR_grant_table_op" hypervisorGrantTableOp :: Word32 -> Ptr a -> Word32 -> IO ()
foreign import ccall "mini-os/gnttab.h map_frames" mapFrames :: Ptr Word64 -> Word64 -> IO (Ptr a)

foreign import ccall "hs_put_free_entry" putFreeEntry :: GrantRef -> IO ()
foreign import ccall "hs_set_xen_guest_handle" setXenGuestHandle :: Ptr GnttabSetupTable -> Ptr Word32 -> IO ()
foreign import ccall "hs_set_gnttab_table" setGnttabTable :: Ptr a -> IO ()

foreign export ccall "init_gnttab" initGnttab :: IO ()

nrGrantFrames :: Word32
nrGrantFrames     = 4
nrReservedEntries = 8
nrGrantEntries    = nrGrantFrames * pageSize `div` entrySize

initGnttab :: IO ()
initGnttab = do (frames :: Ptr Word32) <- mallocArray $ fromInteger $ toInteger nrGrantFrames
                mapM_ (putFreeEntry.fromInteger.toInteger) [nrReservedEntries..nrGrantFrames-1]
                let setup = GnttabSetupTable {
                  gnttabSetupTableDom = domidSelf,
                  gnttabSetupTableNrFrames = nrGrantFrames,
                  gnttabSetupTableStatus = 0,
                  gnttabSetupTableFrameList =  nullPtr
                }
                -- todo: pass a pointer
                ptr <- toC setup
                setXenGuestHandle ptr frames
                hypervisorGrantTableOp opSetupTable ptr 1
                grantTable <- mapFrames (castPtr frames) $ fromInteger $ toInteger nrGrantFrames
                setGnttabTable grantTable
                printk $ "gnttab_table mapped at" ++ show grantTable ++ ".\n"
