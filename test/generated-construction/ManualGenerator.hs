{-# LANGUAGE TemplateHaskell #-}
module ManualGenerator (manualEq) where
import Language.Haskell.TH

manualEq :: Name -> Bool -> Q [Dec]
manualEq target compareFields = do
    a <- newName "a"
    x <- newName "x"
    y <- newName "y"
    let operand = AppT (ConT target) (VarT a)
        body = if compareFields
            then InfixE (Just (VarE x)) (VarE '(==)) (Just (VarE y))
            else ConE 'True
        pats = if compareFields
            then [ConP (mkName "S") [] [VarP x], ConP (mkName "S") [] [VarP y]]
            else [WildP, WildP]
    pure [InstanceD (Just Overlappable) [] (AppT (ConT ''Eq) operand)
        [FunD '(==) [Clause pats (NormalB body) []]]]
