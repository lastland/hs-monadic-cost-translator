module Generated where
data Exports = Exports { invoke :: Int -> Int }
prepare = let fM x = x in Exports fM
