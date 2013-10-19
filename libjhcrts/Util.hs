module Util(printk, rawPrintk, castNum) where
import Foreign.C.String
foreign import ccall "console.h printk" rawPrintk :: CString -> IO ()

printk :: String -> IO ()
printk  = flip withCString rawPrintk

castNum :: (Num a, Integral b) => b -> a
castNum = fromInteger.toInteger
