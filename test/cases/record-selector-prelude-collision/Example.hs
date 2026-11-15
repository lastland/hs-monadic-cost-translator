module Example where
import Prelude hiding (map)
data R = R { map :: Int }
observed x = map (R x)
