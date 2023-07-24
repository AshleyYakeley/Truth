module Pinafore.Documentation
    ( module Pinafore.Text
    , DefDoc(..)
    , DocItem(..)
    , operatorFixity
    , typeOperatorFixity
    , Fixity(..)
    , FixAssoc(..)
    , Name(..)
    , ToText(..)
    , nameIsInfix
    , allKeywords
    , allOperatorNames
    , allTypeNames
    ) where

import Pinafore.Language.DefDoc
import Pinafore.Language.Grammar
import Pinafore.Language.Library
import Pinafore.Language.Library.Defs
import Pinafore.Language.Name
import Pinafore.Text
import Shapes

allOperatorNames :: (DocItem -> Bool) -> [Name]
allOperatorNames test = let
    getDocName :: forall context. BindDoc context -> Maybe Name
    getDocName MkBindDoc {bdScopeEntry = Just (BindScopeEntry (MkFullNameRef name _) _ _), bdDoc = dd}
        | test $ docItem dd
        , nameIsInfix name = Just name
    getDocName _ = Nothing
    in mapMaybe getDocName $ builtInLibrary >>= libraryModuleEntries

bindDocTypeName :: BindDoc a -> [Name]
bindDocTypeName bd =
    case docItem (bdDoc bd) of
        TypeDocItem {diNames = n} -> fmap fnrName $ toList n
        _ -> []

libraryTypeNames :: LibraryModule a -> [Name]
libraryTypeNames lm = toList (lmContents lm) >>= bindDocTypeName

allTypeNames :: [Name]
allTypeNames = filter (not . nameIsInfix) $ sort $ nub $ builtInLibrary >>= libraryTypeNames
