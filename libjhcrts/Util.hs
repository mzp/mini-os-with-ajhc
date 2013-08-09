module Util(printk, printk2) where
import Foreign.C.String
foreign import ccall "console.h printk" printk :: BitsPtr_ -> IO ()
foreign import ccall "console.h printk" printk2 :: CString -> IO ()
