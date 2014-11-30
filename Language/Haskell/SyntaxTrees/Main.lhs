> {-
>    Module      : Language.Haskell.SyntaxTrees
>    Copyright   : (c) Dominic Orchard 2010
>    License     : BSD3
>    Maintainer  : Dominic Orchard <dom.orchard@gmail.com>
>    Stability   : experimental
>    Portability : portable (tempalte-haskell)
> -}

> {-# LANGUAGE MultiParamTypeClasses #-}

> module Language.Haskell.SyntaxTrees.Main where

> type Witness a = a

> class Translation s t where
>     {-| Translate a tree of type @s@ to a tree of type @t@.

>       If translation fails then @translate s = Left s@, otherwise
>       @translate s = Right t@ where @t@ is the translated tree. -}
>     translateTree :: s -> Either s t
>     {-| Parse a string to a tree of type @t@, via intermediate representation
>       as a tree of type @s@. Requires a witness of the intermediate type @s@
>       to be passed as the first argument.

>       If parsing fails then @parseToTarget s = Left s@, otherwise
>       @parseToTarget s = Right t@ where @t@ is the parsed tree. -}
>     parseToTarget :: Witness s -> String -> Either String t

