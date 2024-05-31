module Pinafore.Documentation
    ( module Pinafore.Syntax.Text
    , module Pinafore.Syntax.Name
    , module Pinafore.Syntax.Doc
    , module Pinafore.Main
    , getModuleDocs
    , operatorFixity
    , typeOperatorFixity
    , Fixity(..)
    , FixAssoc(..)
    , allKeywords
    , allOperatorNames
    , allTypeNames
    ) where

import Import
import Pinafore.Context
import Pinafore.Language
import Pinafore.Language.Library
import Pinafore.Language.Library.LibraryModule
import Pinafore.Main
import Pinafore.Syntax.Doc
import Pinafore.Syntax.Name
import Pinafore.Syntax.Text

allOperatorNames :: (DocItem -> Bool) -> [Name]
allOperatorNames test = let
    getDocName :: forall context. BindDoc context -> Maybe Name
    getDocName MkBindDoc {bdScopeEntry = Just (BindScopeEntry (MkFullNameRef name _) _ _), bdDoc = dd}
        | test $ docItem dd
        , nameIsInfix name = Just name
    getDocName _ = Nothing
    in sort $
       nub $
       mapMaybe getDocName $ do
           lmod <- builtInLibrary
           libraryContentsEntries $ lmContents lmod

bindDocTypeName :: BindDoc a -> [Name]
bindDocTypeName bd =
    case docItem (bdDoc bd) of
        TypeDocItem {diNames = n} -> fmap fnrName $ toList n
        _ -> []

libraryTypeNames :: LibraryModule a -> [Name]
libraryTypeNames lm = toList (lmContents lm) >>= bindDocTypeName

allTypeNames :: [Name]
allTypeNames = filter (not . nameIsInfix) $ sort $ nub $ builtInLibrary >>= libraryTypeNames

getModuleDocs :: (?library :: LibraryContext) => ModuleName -> IO Docs
getModuleDocs modname = do
    qmodule <- fromInterpretResult $ runPinaforeScoped "<doc>" $ getModule modname
    return $ moduleDoc qmodule
