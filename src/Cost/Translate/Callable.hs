module Cost.Translate.Callable (callArity, emitCallable, isComposition, spine) where

import GHC
import GHC.Types.Name (getOccString, nameModule_maybe)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import Control.Monad (unless, replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import qualified Cost.Translate.Output as O

type Resolver = Name -> Maybe (O.Expr, Int)
data Head = Named Name | Composed (LHsExpr GhcRn) (LHsExpr GhcRn)
data Callable = Call O.Expr Int [O.Expr] | Compose Callable Callable
type Build = StateT Int (Either String)

isComposition :: Name -> Bool
isComposition n = getOccString n == "." && maybe False
    ((== "GHC.Internal.Base") . moduleNameString . moduleName) (nameModule_maybe n)

spine :: LHsExpr GhcRn -> (LHsExpr GhcRn, [LHsExpr GhcRn])
spine expr = case unLoc expr of
    HsApp _ f a -> let (fun,args) = spine f in (fun,args ++ [a])
    HsPar _ e -> spine e
    ExprWithTySig _ e _ -> spine e
    _ -> (expr, [])

view :: LHsExpr GhcRn -> Either String (Head, [LHsExpr GhcRn])
view expr = case spine expr of
    (L _ (HsVar _ n), f:g:args) | isComposition (unLoc n) -> Right (Composed f g,args)
    (L _ (OpApp _ f op g), args) | HsVar _ n <- unLoc op, isComposition (unLoc n) -> Right (Composed f g,args)
    (L _ (HsVar _ n), args) | not (isComposition (unLoc n)) -> Right (Named (unLoc n),args)
    (L _ (HsRecSel _ field), args) -> Right (Named (foExt field),args)
    _ -> failure expr "function alias or computed function result has no validated calling convention"

failure :: LHsExpr GhcRn -> String -> Either String a
failure expr message = Left (showSDocUnsafe (ppr (getLocA expr)) ++ ": " ++ message)

callArity :: Resolver -> LHsExpr GhcRn -> Either String Int
callArity resolve expr = do
    (fun,args) <- view expr
    count <- case fun of
        Named n -> maybe (failure expr "function alias or computed function result has no validated calling convention") (Right . snd) (resolve n)
        Composed f g -> do
            nf <- callArity resolve f
            ng <- callArity resolve g
            unless (nf > 0 && ng > 0) (failure expr "composition needs a named function right operand and a named result convention")
            pure nf
    unless (length args <= count) (failure expr "application exceeds the validated argument count")
    pure (count - length args)

-- Prepare captured arguments once, then specialize a composition when its
-- first argument arrives. Flattening it into a lambda and only allocating the
-- intermediate at saturation would charge g x twice in map ((f . g) x) ys.
emitCallable :: Resolver -> (LHsExpr GhcRn -> Either String O.Expr) -> LHsExpr GhcRn -> Either String O.Expr
emitCallable resolve render expr = do
    _ <- callArity resolve expr
    evalStateT (do (prefix,fun) <- prepare expr; result <- finish fun; pure (O.doE prefix result)) 0
  where
    prepare e = do
        (headForm,args) <- lift (view e)
        (prefix,fun) <- case headForm of
            Named n -> case resolve n of
                Just (name,count) -> pure ([], Call name count [])
                Nothing -> lift (failure e "unsupported callable")
            Composed f g -> do
                (pf,cf) <- prepare f
                (pg,cg) <- prepare g
                pure (pf ++ pg, Compose cf cg)
        actions <- lift (mapM render args)
        names <- replicateM (length args) fresh
        (more,applied) <- specialize fun (map O.v names)
        pure (prefix ++ zipWith suspend names actions ++ more, applied)
    specialize (Call name n captured) args = pure ([], Call name (n-length args) (captured ++ args))
    specialize composed@(Compose _ _) [] = pure ([], composed)
    -- The first input flows through the right operand once. A unary right
    -- operand saturates and its data result is suspended as a thunk. A
    -- multi-arity right operand stays partially applied: its function value
    -- is suspended instead and consumed by the left operand's convention,
    -- such as a mapped callback. Either way it is captured exactly once.
    specialize (Compose f g) (input:rest) = do
        (pg,cg) <- specialize g [input]
        gAction <- finish cg
        middle <- fresh
        (pf,cf) <- specialize f (O.v middle:rest)
        pure (suspend middle (O.doE pg gAction) : pf, cf)
    finish (Call name 0 args) = pure (O.app name args)
    finish (Call name _ args) = pure (O.ret (O.app name args))
    finish fun@(Compose f _) = do
        args <- replicateM (arity f) fresh
        (prefix,applied) <- specialize fun (map O.v args)
        body <- finish applied
        pure (O.ret (O.lam (map O.p args) (O.doE prefix body)))
    arity (Call _ n _) = n
    arity (Compose f _) = arity f

fresh :: Build String
fresh = do
    n <- get
    put (n+1)
    pure ("partial" ++ show n)

suspend :: String -> O.Expr -> O.Stmt
suspend name action = O.bind name (O.call "M.lazy" [action])
