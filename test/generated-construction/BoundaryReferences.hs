module BoundaryReferences where

import qualified Memorist.Base.Core as Core
import qualified Memorist.Base.ANT as ANT

evaluateReference :: Core.Memorist s m => m a -> s -> Core.Annotated a
evaluateReference = Core.eval

evaluateInitialReference :: Core.Memorist s m => m a -> Core.Annotated a
evaluateInitialReference = Core.eval1

extractReference :: Core.Annotated a -> a
extractReference = Core.value

runReference :: Core.Memorist s m => m a -> s -> (Core.Annotated a, s)
runReference = Core.runM

baseRunnerPassed, baseProjectionPassed, annotationProjectionPassed, ancestralProjectionPassed :: ()
baseRunnerPassed = receive Core.runMMBT
baseProjectionPassed = receive Core.runMBT
annotationProjectionPassed = receive Core.runAnnotT
ancestralProjectionPassed = receive ANT.runMT

receive :: a -> ()
receive _ = ()
