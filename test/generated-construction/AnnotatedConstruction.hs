module AnnotatedConstruction where

import qualified Memorist.Memorist as M

construct :: a -> M.Annot -> M.Annotated a
construct = M.Annotated
