module Cost.Translate.Diagnostic (Diagnostic(..), renderDiagnostic) where

data Diagnostic = UnsupportedSource String | GeneratedModuleNameInvalid String

renderDiagnostic :: Diagnostic -> String
renderDiagnostic (UnsupportedSource message) = "unsupported source: " ++ message
renderDiagnostic (GeneratedModuleNameInvalid candidate) =
    "generated module name is invalid before publication\nFailed candidate:\n" ++ candidate
