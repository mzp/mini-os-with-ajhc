module Gnttab where
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Data.Word
import Util
import GnttabStub
import Struct
import Xen

type GrantRef= Int32
foreign import ccall "hs_get_gnttab_list" getGnttabList :: IO (Ptr GrantRef)
foreign import ccall "hs_set_xen_guest_handle" setXenGuestHandle :: Ptr GnttabSetupTable -> Ptr Word32 -> IO ()
foreign import ccall "hs_set_gnttab_table" setGnttabTable :: Ptr a -> IO ()
foreign import ccall "hs_get_gnttab_sem" getGnttabSem :: IO (Ptr Word8)
foreign export ccall "init_gnttab" initGnttab :: IO ()
foreign export ccall "put_free_entry" putFreeEntry :: GrantRef -> IO ()

nrGrantFrames :: Word32
nrGrantFrames     = 4
nrReservedEntries = 8
nrGrantEntries    = nrGrantFrames * pageSize `div` entrySize

initGnttab :: IO ()
initGnttab = do (frames :: Ptr Word32) <- mallocArray $ fromInteger $ toInteger nrGrantFrames
                mapM_ (putFreeEntry.fromInteger.toInteger) [nrReservedEntries..nrGrantEntries-1]
                let setup = GnttabSetupTable {
                  gnttabSetupTableDom = domidSelf,
                  gnttabSetupTableNrFrames = nrGrantFrames,
                  gnttabSetupTableStatus = 0
                }
                -- todo: pass a pointer
                ptr <- toC setup
                setXenGuestHandle ptr frames
                hypervisorGrantTableOp opSetupTable ptr 1
                grantTable <- mapFrames (castPtr frames) $ fromInteger $ toInteger nrGrantFrames
                setGnttabTable grantTable
                printk $ "gnttab_table mapped at" ++ show grantTable ++ ".\n"

putFreeEntry :: GrantRef -> IO ()
putFreeEntry ref =
  do flags <- localIrqSave 0
     gnttabList <- getGnttabList
     let n = fromInteger $ toInteger ref
     v0 <- peekElemOff gnttabList 0
     pokeElemOff gnttabList n v0
     pokeElemOff gnttabList 0 ref
     localIrqRestore flags
     up =<< getGnttabSem
     return ()
