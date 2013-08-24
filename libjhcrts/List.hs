module List where
import Foreign.Ptr
import Data.Maybe

newtype CList   = CList ()
foreign import ccall "mini-os/list.h minios_list_add_tail" miniosListAddTail :: Ptr CList -> Ptr CList -> IO ()
foreign import ccall "mini-os/list.h minios_list_del" miniosListDel :: Ptr CList -> IO ()
foreign import ccall "hs_minios_init_list_head" miniosInitListHead :: Ptr CList -> IO ()
foreign import ccall "hs_list_next" getNext :: Ptr CList -> IO (Ptr CList)

toList :: Ptr CList -> IO [ Ptr CList ]
toList p = do pos <- getNext p
              n   <- getNext pos
              loop pos n
  where loop pos n  = if pos == p then
                        return []
                      else
                         do n' <- getNext n
                            ns <- loop n n'
                            return $ pos : ns
