{-# LANGUAGE GADTs #-}
module Generated (BoxT(MakeBox), NewBox(..), GBox(..), Standalone, Hidden, Ambiguous(..), Other) where

data BoxT = MakeBox Int | PrivateBox Int
newtype NewBox = NewBox Int
data GBox where
    GBox :: Int -> GBox
data Hidden = Hidden Int
data Ambiguous = Other Int
data Other = Ambiguous Int
data StandaloneType = Standalone Int
