module Cost.Translate.Output.Patterns
    ( p
    , conP
    , conRecordP
    ) where

import Cost.Translate.Output.Ast (Pat, rdr)
import GHC hiding (Pat, Stmt)
import GHC.Types.Basic (appPrec)
import GHC.Types.Name.Occurrence (dataName, varName)

p :: String -> Pat
p "_" = noLocA (WildPat noExtField)
p name = noLocA (VarPat noExtField (noLocA (rdr varName name)))

conP :: String -> [Pat] -> Pat
conP name ps =
    noLocA
        ( ConPat
            noAnn
            (noLocA (rdr dataName name))
            (PrefixCon [] (map (parenthesizePat appPrec) ps))
        )

conRecordP :: String -> Pat
conRecordP name =
    noLocA
        ( ConPat
            noAnn
            (noLocA (rdr dataName name))
            (RecCon (HsRecFields [] Nothing))
        )
