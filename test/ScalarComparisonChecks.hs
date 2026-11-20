module ScalarComparisonChecks (checkScalarComparisonCallers) where

import ReferenceRunner (Artifacts, ReferenceEnvironment, checkOptimizedReferenceCases)

-- The ordinary lifecycle already checked the whole publication and compared
-- it at the default optimization. Rebuild the same caller/reference artifacts,
-- without changing their probes or the handwritten value/cost oracle.
checkScalarComparisonCallers :: ReferenceEnvironment -> [Artifacts] -> IO ()
checkScalarComparisonCallers = checkOptimizedReferenceCases "caller scalar comparisons"
