{-# LANGUAGE TypeFamilies #-}
module MappedSupport where
import qualified Memorist.Memorist as M

data Source = Source

instance M.Repr Source where
    type Rep Source = M.Th Source
    reprM _ = M.lazy (pure Source)
{-# LINE 50 "SharedSupport.hs" #-}
    abstract t = M.tval t
