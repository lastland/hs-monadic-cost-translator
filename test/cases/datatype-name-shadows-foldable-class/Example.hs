module Example where
import Prelude hiding (Foldable)
data Foldable a = Box a
unbox (Box x) = x
observed x = unbox (Box x)
