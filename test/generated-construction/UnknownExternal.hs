module UnknownExternal where

import qualified Type.Reflection as Reflection

reflection :: Reflection.Typeable a => proxy a -> Reflection.SomeTypeRep
reflection = Reflection.someTypeRep
