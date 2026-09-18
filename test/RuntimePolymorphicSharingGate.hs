{-# LANGUAGE RankNTypes #-}
module Main (main) where
import Memorist.Memorist
import Memorist.Data.List
import qualified Memorist.Base.Thunk as MT
import Control.Monad (unless)

-- Reserve one name using the external allocator; let-generalize the thunk
-- obtained by evaluating the same allocator at that reserved name. No custom
-- thunk constructor, unsafe cast, or replacement runtime is involved.
sharedAt :: MemoristM (Th a) -> Th () -> Th a
sharedAt action anchor = value (eval action (tname anchor))

emptyM :: MemoristM (ListT a)
emptyM = tick >> pure NilT

program :: MemoristM Int
program = do
    anchor <- lazy (pure ())
    let shared :: forall a. Th (ListT a)
        shared = sharedAt (lazy emptyM) anchor
    a <- force (shared :: Th (ListT Int))
    b <- force (shared :: Th (ListT Bool))
    case (a,b) of
        (NilT,NilT) -> pure 5
        _ -> error "unexpected constructor"

main :: IO ()
main = do
    unless (correct program (5::Int) && MT.toInteger (infcost program fullDemand) == 1)
        (fail "polymorphic binding must share its one entry tick")
    putStrLn "PASS: one reserved shared thunk at Int and Bool, value 5, cost 1"
