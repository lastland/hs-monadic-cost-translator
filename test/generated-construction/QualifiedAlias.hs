module QualifiedAlias where
import qualified Memorist.Memorist as Reasoning
extract :: Reasoning.Annotated Int -> Int
extract = Reasoning.value
