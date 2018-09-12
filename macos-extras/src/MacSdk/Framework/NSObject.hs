{-# LANGUAGE TypeFamilies #-}

module MacSdk.Framework.NSObject where

import Foreign

class NSClass c where
class NSObject obj where
  type NSObj obj
  fromNSPtr :: ForeignPtr (NSObj obj) -> obj
  toNSPtr   :: obj -> ForeignPtr (NSObj obj)

instance NSObject (ForeignPtr c) where
  type NSObj (ForeignPtr c) = c
  fromNSPtr = id
  toNSPtr = id

data NSObject_
instance NSClass NSObject_
newtype Object = Object { getObject :: ForeignPtr NSObject_ } deriving NSObject

foreign import ccall unsafe ns_retain :: Ptr NSObject_ -> IO ()
foreign import ccall unsafe ns_release :: Ptr NSObject_ -> IO ()

withNSPtr :: NSObject obj => obj -> (Ptr (NSObj obj) -> IO a) -> IO a
withNSPtr obj = withForeignPtr (toNSPtr obj)

manageNSPtr :: NSObject obj => Ptr (NSObj obj) -> IO obj
manageNSPtr = fmap fromNSPtr . newForeignPtr_
