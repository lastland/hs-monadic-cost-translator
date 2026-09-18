module Cost.Translate.Diagnostic (Diagnostic(..), renderDiagnostic) where

-- Preserve phase information at the frontend/publication boundary. Internal
-- validation helpers can still return ordinary source-located explanations.
data Diagnostic = UnsupportedSource String | GeneratedModuleNameInvalid String

renderDiagnostic :: Diagnostic -> String
renderDiagnostic (UnsupportedSource message) = "unsupported source: " ++ message
renderDiagnostic (GeneratedModuleNameInvalid candidate) =
    "generated module name is invalid before publication\nFailed candidate:\n" ++ candidate
