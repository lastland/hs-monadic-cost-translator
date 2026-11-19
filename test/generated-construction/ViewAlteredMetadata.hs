module ViewAlteredMetadata where

import qualified Memorist.Memorist as M

changed :: (a -> b) -> M.Th a -> M.Th b
changed convert (M.Th name payload metadata) = M.Th (M.nnext name) (convert payload) metadata

cross :: (a -> b) -> M.Th a -> M.Th a -> M.Th b
cross convert (M.Th name payload metadata) (M.Th other value otherMetadata) = M.Th name (convert payload) otherMetadata
