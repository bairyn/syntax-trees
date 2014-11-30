> {-
>    Module      : Language.Haskell.SyntaxTrees
>    Copyright   : (c) Dominic Orchard 2010
>    License     : BSD3
>    Maintainer  : Dominic Orchard <dom.orchard@gmail.com>
>    Stability   : experimental
>    Portability : portable (template-haskell)
> -}

> {-# LANGUAGE MultiParamTypeClasses #-}

> {-| Provides an instance that translates
>   haskell-src-exts expression trees into Template Haskell expression
>   trees in a way that depends only on the haskell-src-exts syntax tree
>   and agreement on the pretty-printed representation of
>   Haskell between haskell-src-exts pretty-printer and
>   Template Haskell quotations (as opposed to depending on
>   both TH and haskell-src-exts syntax tree representations).
>
>
>   Instead of converting between data types,
>   haskell-src-exts syntax trees are pretty-printed and wrapped in
>   a TH quotation which is then interpreted as a Haskell program,
>   yielding a TH Exp tree. Free variables in the haskell-src-exts tree are
>   preserved by lifting them to TH splices prior to pretty-printing.
>
>   e.g. @parseToTH \"let x = 1 in x + y\"@ =
>   @
>       Right (LetE [ValD (VarP x_1) (NormalB (LitE (IntegerL 1))) []]
>         (InfixE (Just (VarE x_1)) (VarE GHC.Num.+) (Just (VarE y))))
>   @
> -}

> module Language.Haskell.SyntaxTrees.ExtsToTH (
>     translateExtsToTH,
>     parseToTH,
>     parseToTarget,
>     translateTree) where

> import Language.Haskell.SyntaxTrees.Main

> import Language.Haskell.Interpreter hiding (name)
> import Language.Haskell.Exts.Parser
> import Language.Haskell.Exts.Pretty
> import qualified Language.Haskell.Exts.Syntax as Exts
> import qualified Language.Haskell.TH as TH
> import Data.Data
> import Data.Generics.Uniplate.Data
> import Control.Monad.State.Lazy
> import qualified Control.Monad.State.Class as State
> import System.IO.Unsafe (unsafePerformIO)

================================================================================

> instance Translation Exts.Exp TH.Exp where
>   translateTree t = unsafePerformIO $
>              do mx <- runInterpreter (interpretTH (buildTHString t))
>                 case mx of
>                   Left _ -> (return . Left) t
>                   Right x -> x >>= (return . Right)
>
>   parseToTarget _witness s = case (parseExp s) of
>                 ParseOk ex -> case (translateTree ex) of
>                                Left _ -> Left s
>                                Right x -> Right x
>                 ParseFailed _ string -> Left $ s++string


> {-| Translate a Language.Haskell.Exts.Exp (haskell-src-exts) syntax tree
>     to a Language.Haskell.TH.Exp (template-haskell) syntax tree -}
> translateExtsToTH :: Exts.Exp -> Either Exts.Exp TH.Exp
> translateExtsToTH = translateTree

> {-| Parse a string to a Language.Haskell.TH.Exp (template-haskell) expression
>   via intermediate representation as a Exts.Exp tree. -}
> parseToTH :: String -> Either String TH.Exp
> parseToTH = parseToTarget (undefined::Exts.Exp)

================================================================================

Build the template Haskell quote expresion

> buildTHString :: Exts.Exp -> String
> buildTHString t = "(runQ [|" ++ (prettyPrint $ liftFreeVars t) ++ "|])::IO Exp"

> interpretTH :: String -> Interpreter (IO TH.Exp)
> interpretTH thString = do
>    set [languageExtensions := [TemplateHaskell]]
>    setImports ["Prelude","Language.Haskell.TH","Language.Haskell.TH.Syntax"]
>    interpret thString (undefined::(IO TH.Exp))

================================================================================

Free variables must be lifted into TemplateHaskell splices, as free variables are not
allowed in a TemplateHaskell quotation. For example:

    [| x |] -> [| $( varE (mkName "x"))$ |]

liftFreeVars processes an Exts.Exp expression, keeping an account in its state of
scoped variables in the following nested scope structure:

> data NestedScopes = Next [String] NestedScopes | Empty deriving Show

isFree is used to ask if an encountered variable name is currently free

> isFree :: String -> NestedScopes -> Bool
> isFree _    Empty = True
> isFree var (Next vars n) = (not $ elem var vars) && isFree var n

Any free variables are converted into an Exts.Exp of a TH splice by mkFreeVar:

> mkFreeVar :: Exts.Name -> Exts.Exp
> mkFreeVar n = Exts.SpliceExp $ Exts.ParenSplice $
>             (Exts.App (Exts.Var (Exts.UnQual $ Exts.Ident "varE"))
>                       (Exts.App (Exts.Var $ Exts.UnQual $ Exts.Ident "mkName")
>                                 (Exts.Lit $ Exts.String $ nameToString $ n)))


For every binder (e.g. lambdas, lets) we create a new scope in our state
and add the bound variables. The following extract variable names from various
binding forms:

> getPatBinders :: [Exts.Pat] -> [String]
> getPatBinders = concatMap getPatBinders'
>   where
>   getPatBinders' p =
>       [nameToString name | (Exts.PVar name)  <- universe p]

> getBinders :: Exts.Binds -> [String]
> getBinders (Exts.BDecls decls) = concatMap getBinders' decls

>   where getBinders' p = concat ((patBinders p) ++ (matchBinders p))

>         patBinders p = [(getPatBinders [pat])++(getBinders binds) |
>                               (Exts.PatBind _ pat _ binds) <- universe p]

>         matchBinders p = [concatMap getMatchBinders matches |
>                               (Exts.FunBind matches) <- universe p]
> getBinders (Exts.IPBinds ipbinds) = map getIPBinders ipbinds

> getMatchBinders :: Exts.Match -> [String]
> getMatchBinders (Exts.Match _ _ pats _ _ binds) = getPatBinders pats ++ getBinders binds

> getAltBinders :: [Exts.Alt] -> [String]
> getAltBinders = concatMap getAltBinders'
>   where
>   getAltBinders' :: Exts.Alt -> [String]
>   getAltBinders' (Exts.Alt _ pat _ binds) = getPatBinders [pat] ++ getBinders binds


> getIPBinders :: Exts.IPBind -> String
> getIPBinders (Exts.IPBind _srcLine (Exts.IPDup n) _expr) = n
> getIPBinders (Exts.IPBind _srcLine (Exts.IPLin n) _expr) = n

liftFreeVars processes the tree town down using the uniplate extension:
transformTopDownM (see below) to process the tree top down and to not process newly
generated subtrees.

> liftFreeVars :: Exts.Exp -> Exts.Exp
> liftFreeVars exprTree = evalState (transformTopDownM f exprTree) Empty
>   where
>     -- Qualified variable encountered, transformation occurs here**
>     -- Qualified variables are always 'free'
>     f (Exts.Var (Exts.Qual (Exts.ModuleName m) name)) = do
>           return $ Right $ mkFreeVar $ Exts.Ident $ m ++ "." ++ nameToString name
>     -- Variable encountered: *** transformation occurs here**
>     f e@(Exts.Var (Exts.UnQual name)) = do
>         scopedVars <- State.get
>         if (isFree (nameToString name) scopedVars) then
>             return $ Right (mkFreeVar name)
>          else
>             return $ Left e
>     -- Let binder
>     f e@(Exts.Let binds _expr) = do
>         scopedVars <- State.get
>         State.put $ Next (getBinders binds) scopedVars
>         return $ Left e

>     -- Lambda binder
>     f e@(Exts.Lambda _srcLine pats _expr) = do
>         scopedVars <- State.get
>         State.put $ Next (getPatBinders pats) scopedVars
>         return $ Left e

>     -- Case binder
>     f e@(Exts.Case _expr alts) = do
>         scopedVars <- State.get
>         State.put $ Next (getAltBinders alts) scopedVars
>         return $ Left e

>     -- Arrow Proc binder
>     f e@(Exts.Proc _srcLine pat _expr) = do
>         scopedVars <- State.get
>         State.put $ Next (getPatBinders [pat]) scopedVars
>         return $ Left e

>     -- Arrow Proc binder
>     f (Exts.InfixApp exp1 qop exp2) = do
>         return $ Left $ Exts.App (Exts.App (Exts.Var (qOpToQName qop)) exp1) exp2

>     -- Default case
>     f x = return $ Left x


 blankSrcLoc = Exts.SrcLoc "" 0 0
 snLoc x = Exts.SrcLoc x 0 0

> qOpToQName :: Exts.QOp -> Exts.QName
> qOpToQName (Exts.QVarOp qnam) = qnam
> qOpToQName (Exts.QConOp qnam) = qnam

> nameToString :: Exts.Name -> String
> nameToString (Exts.Ident  s) = s
> nameToString (Exts.Symbol s) = s


================================================================================

Extension to Uniplate

Use descendM to do a top-down parse of a uniplatable tree,
but take a function with return type: m (Either on on), with behaviour:
    Descend on a value of Left x
    Stop on a value of Right x, which denotes that a new sub-tree was built that
       shouldn't be parsed over.


> transformTopDownM :: (Monad m, Uniplate on, Data on) => (on -> m (Either on on)) -> on -> m on
> transformTopDownM f = g
>    where g x = do x' <- (f x)
>                   case x' of
>                     Left x'' -> ((descendM g) =<< (return x''))
>                     Right x'' -> return x''
