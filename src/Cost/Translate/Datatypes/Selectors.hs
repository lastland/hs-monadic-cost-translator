module Cost.Translate.Datatypes.Selectors (selectors, selectorDefinitions) where

import Cost.Translate.Datatypes.Constructor (patternFor)
import Cost.Translate.Datatypes.Types (renderType)
import Cost.Translate.Kinds (renderBinder, visibleVariables)
import Cost.Translate.Names (generatedName)
import Cost.Translate.Output qualified as O
import Data.List (nub)
import GHC
import GHC.Core.DataCon (dataConOrigArgTys)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Types.FieldLabel (flSelector)
import GHC.Types.Name (getOccString)

selectors :: [TyCon] -> [(Name, String)]
selectors types =
    [ (flSelector f, generatedName "M" (flSelector f))
    | t <- types
    , c <- tyConDataCons t
    , f <- dataConFieldLabels c
    ]

selectorDefinitions :: [TyCon] -> Either String [O.Decl]
selectorDefinitions types = concat <$> mapM (forType types) types

forType :: [TyCon] -> TyCon -> Either String [O.Decl]
forType types tc = do
    quantified <- mapM (renderBinder types) (tyConTyVars tc)
    let vs = map (O.t . getOccString) (visibleVariables tc)
        recordType = O.tc (generatedName "T" (getName tc)) vs
        fields =
            nub
                [ (flSelector field, generatedName "M" (flSelector field))
                | con <- tyConDataCons tc
                , field <- dataConFieldLabels con
                ]
    concat <$> mapM (emitSelector types quantified recordType tc) fields

emitSelector ::
    [TyCon] ->
    [O.Binder] ->
    O.Ty ->
    TyCon ->
    (Name, String) ->
    Either String [O.Decl]
emitSelector types quantified recordType tc (name, generated) = do
    fieldTy <-
        maybe missing Right (selectorType tc name)
    rendered <- renderType types fieldTy
    let signature = O.forallT quantified (O.arrow thunkType resultType)
        thunkType = O.tc "M.Th" [recordType]
        resultType = O.tc "M.MemoristM" [rendered]
        fallback =
            [ (O.p "_", failure)
            | not (total tc name)
            ]
        body = selectorBody (selectorCases tc name ++ fallback)
    pure [O.sig generated signature, O.val generated [O.p "recordThunk"] body]
    where
        missing = Left "missing record selector field type"
        failure = O.call "error" [O.string "source record selector failure"]

selectorCases :: TyCon -> Name -> [(O.Pat, O.Expr)]
selectorCases tc name =
    [ ( patternFor
            (generatedName "T" (getName con))
            [if i == index then fieldName else "_" | i <- [0 .. fieldCount - 1]]
      , O.call "M.force" [O.v fieldName]
      )
    | con <- tyConDataCons tc
    , let labels = map flSelector (dataConFieldLabels con)
    , (index, field) <- zip [0 :: Int ..] labels
    , field == name
    , let fieldCount = length (dataConOrigArgTys con)
    , let fieldName = "field" ++ show index
    ]

selectorBody :: [(O.Pat, O.Expr)] -> O.Expr
selectorBody cases =
    O.doE
        [ O.thenS (O.ann (O.v "M.tick") (O.tc "M.MemoristM" [O.t "()"]))
        , O.bind "recordValue" (O.call "M.force" [O.v "recordThunk"])
        ]
        (O.caseE (O.v "recordValue") cases)

selectorType :: TyCon -> Name -> Maybe Type
selectorType tc name = case matches of
    ty : _ -> Just ty
    [] -> Nothing
    where
        matches =
            [ scaledThing ty
            | con <- tyConDataCons tc
            , let fields = dataConOrigArgTys con
            , let labels = map flSelector (dataConFieldLabels con)
            , (field, ty) <- zip labels fields
            , field == name
            ]

total :: TyCon -> Name -> Bool
total tc name =
    all (elem name . map flSelector . dataConFieldLabels) (tyConDataCons tc)
