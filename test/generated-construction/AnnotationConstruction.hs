module AnnotationConstruction where

import qualified Memorist.Memorist as M

unused :: a -> M.Annot -> M.MemoristM a
unused = M.annotate
