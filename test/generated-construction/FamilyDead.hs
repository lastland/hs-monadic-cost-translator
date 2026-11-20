{-# LANGUAGE ConstraintKinds, FlexibleContexts, TypeFamilies, UndecidableInstances #-}
module FamilyDead where

import Data.Kind (Type)
import FamilySupport (ReprReq)
import qualified Memorist.Memorist as M

data Dead a = Dead (Maybe a)
data DeadT a = DeadT (M.Th (M.MaybeT a))

instance ReprReq Type a => M.Repr (Dead a) where
    type Rep (Dead a) = DeadT (M.Rep a)
    reprM (Dead x) =
        let unused t = M.abstract t :: Maybe Int
        in DeadT <$> M.lazy (M.reprM x)
    abstract (DeadT x) =
        let hidden t = M.tval t
        in Dead (M.abstract (M.tval x))
