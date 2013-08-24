module Util(printk, rawPrintk) where
import Foreign.C.String
foreign import ccall "console.h printk" rawPrintk :: CString -> IO ()

printk :: String -> IO ()
printk  = flip withCString rawPrintk
