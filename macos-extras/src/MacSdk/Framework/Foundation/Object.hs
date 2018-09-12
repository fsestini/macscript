-- | 

module MacSdk.Framework.Foundation.Object where

import Foreign

data NSObject
newtype Object = Object { getObject :: Ptr NSObject }

nullObject :: Object
nullObject = Object nullPtr
