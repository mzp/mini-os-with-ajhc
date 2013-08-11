module Console() where
import XenconsRing
import Util

foreign import ccall "console/console.c hs_set_console_initialised" setConsoleInitialised :: Int -> IO ()
foreign export ccall "init_console" initConsole :: IO ()
initConsole = do printk "Initialising console ... "
                 xenconsRingInit
                 setConsoleInitialised 1
                 printk "done\n"
