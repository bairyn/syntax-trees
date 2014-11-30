> import Fork.Bairyn.Language.Haskell.SyntaxTrees.ExtsToTH
> import qualified Language.Haskell.TH as TH
> import qualified Language.Haskell.Exts.Syntax as Exts

> foo :: Either String TH.Exp
> foo = parseToTH "let x = 1 in x + y"

> foo2 :: Either String TH.Exp
> foo2 = parseToTH "\\x -> (\\y -> (x + y + z))"
