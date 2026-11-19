module PairedPayload where
import HiddenConstruction (hidden)
import qualified Memorist.Memorist as M

payloads :: M.Th Int -> M.Th Int -> (Int, Int)
payloads left right = (hidden left, M.tval right)
