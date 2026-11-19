module DefaultLabels where

class Labels a where
    labels :: a -> [Maybe Bool]
    labels _ = [Nothing, Just True]

instance Labels Bool

ordinary :: [Maybe Bool]
ordinary = labels False
