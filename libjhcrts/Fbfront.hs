module Fbfront where
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Data.Word
import Control.Monad
import Text.Printf
import Util
import FbfrontStub
import Xenbus
import Xen

type Mfns = Ptr Word64
foreign export ccall "_nit_fbfront" initFbfront :: CString -> Mfns -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> IO (Ptr FbfrontDev)
foreign import ccall "hs_get_fbfront_handler" getFbfrontHandler :: IO (FunPtr (IO ()))
foreign import ccall "hs_get_fbfront_dev_evtchn_ptr" getFbfrontDevEvtchnPtr :: Ptr FbfrontDev -> IO (Ptr EvtchnPort)
foreign import ccall "memset" memset :: Ptr a -> Word8 -> Word32 -> IO ()
foreign import ccall "hs_get_max_pd" getMaxPd :: Ptr XenfbPage -> IO Int
foreign import primitive "const.sizeof(unsigned long)" sizeUnsignedLong :: Word32

pageCount :: Int
pageCount = fromInteger $ toInteger $ pageSize `div` sizeUnsignedLong

makeXenfbPage :: Int32 -> Int32 -> Word8 -> Word32 -> Word32 -> IO (Ptr XenfbPage)
makeXenfbPage width height depth stride memLength =
    do s <- allocPage
       setXenfbPageInCons     s 0
       setXenfbPageInProd     s 0
       setXenfbPageOutCons    s 0
       setXenfbPageOutProd    s 0
       setXenfbPageWidth      s width
       setXenfbPageHeight     s height
       setXenfbPageDepth      s depth
       setXenfbPageLineLength s stride
       setXenfbPageMemLength  s memLength
       return s

setupFbfrontDev :: Ptr FbfrontDev -> Ptr XenfbPage -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> IO ()
setupFbfrontDev dev s width height depth stride memLength =
    do setFbfrontDevPage      dev s
       setFbfrontDevWidth     dev width
       setFbfrontDevHeight    dev height
       setFbfrontDevDepth     dev depth
       setFbfrontDevStride    dev stride
       setFbfrontDevMemLength dev memLength
       setFbfrontDevOffset    dev 0
       setFbfrontDevEvents    dev nullPtr

pokeWord64 :: Ptr Word64 -> Int -> Word64 -> IO ()
peekWord64 :: Ptr Word64 -> Int -> IO Word64
pokeWord64 addr i val =
    pokeByteOff addr (i * castNum sizeUnsignedLong) val
peekWord64 addr i =
    peekByteOff addr (i * castNum sizeUnsignedLong)

copyToPd :: Ptr XenfbPage -> Mfns -> Int -> IO ()
copyToPd s mfns n =
    do devPd <- getXenfbPagePd s
       maxPd <- getMaxPd s
       forM_ [0..pageCount-1] (\i->
         do pd <- allocPage
            forM_ [0..pageEntry] (\j ->
              do let mapped = i * pageEntry + j
                 v <- if mapped < n then
                        peekWord64 mfns mapped
                      else
                        return 0
                 pokeWord64 pd j v)
            pokeWord64 devPd i $ virtToMfn pd)
       forM_ [pageCount .. maxPd-1] (\i -> pokeWord64 devPd i 0)
       return ()
  where pageEntry = castNum $ pageSize `div` sizeUnsignedLong
        (m, r) = n `divMod` pageEntry
        isRest = r /= 0
        pageCount = m + if isRest then 1 else 0

initFbfront nodename mfns width height depth stride n =
  do  name <- if nodename == nullPtr then
                return "device/vfb/0"
              else
                peekCString nodename
      printk $ "******************* FBFRONT for " ++ name ++ " **********\n\n\n"
      dev   <- mkFbfrontDev
      name' <- newCString name
      setFbfrontDevNodename dev name'
      let path = name ++ "/backend-id"
      dom <- withCString path xenbusReadInteger
      setFbfrontDevDom dev $ castNum dom
      handler <- getFbfrontHandler
      evtchn  <- getFbfrontDevEvtchnPtr dev
      evtchnAllocUnbound (castNum dom) handler (castPtr dev) evtchn
      s <- makeXenfbPage width height (castNum depth) (castNum stride) (n * pageSize)
      setupFbfrontDev dev s width height depth stride (castNum $ n * pageSize)
      copyToPd s mfns (castNum n)
      xenbusTransaction $ \xbt -> do
        xenbusPrint xbt name "page-ref" $ show $ virtToMfn $ castPtr s
        xenbusPrint xbt name "event-channel" . show =<< getFbfrontDevEvtchn dev
        return True
      return dev

