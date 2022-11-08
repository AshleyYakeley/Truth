module Pinafore.Language.Grammar.Interpret.ScopeBuilder
    ( ScopeBuilder
    , runScopeBuilder
    , Docs
    , defDocs
    , exposeDocs
    , sourcePosScopeBuilder
    , interpScopeBuilder
    , refScopeBuilder
    , pureScopeBuilder
    , allocateVarScopeBuilder
    ) where

import Pinafore.Language.DefDoc
import Pinafore.Language.DocTree
import Pinafore.Language.Grammar.Interpret.RefNotation
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Language.VarID
import Shapes

type Docs = [DocTreeEntry DefDoc]

defDocs :: DefDoc -> Docs
defDocs doc = [EntryDocTreeEntry doc]

exposeDeclids :: [FullName] -> [DefDoc] -> [DefDoc]
exposeDeclids names decls = let
    inDecl :: FullName -> Maybe DefDoc
    inDecl fn = find (diMatchName fn . docItem) decls
    isSubtypeDDI :: DefDoc -> Bool
    isSubtypeDDI doc =
        case docItem doc of
            SubtypeRelationDocItem {} -> True
            _ -> False
    in mapMaybe inDecl names <> filter isSubtypeDDI decls

exposeDocs :: [FullName] -> Docs -> Docs
exposeDocs names = fmap EntryDocTreeEntry . exposeDeclids names . mconcat . fmap toList

type ScopeBuilder = TransformT RefNotation

runScopeBuilder :: ScopeBuilder a -> (a -> RefNotation b) -> RefNotation b
runScopeBuilder sb = unTransformT sb

sourcePosScopeBuilder :: SourcePos -> ScopeBuilder ()
sourcePosScopeBuilder spos = interpScopeBuilder $ refPut (transformParamRef sourcePosParam) spos

interpScopeBuilder :: QScopeInterpreter --> ScopeBuilder
interpScopeBuilder = liftTransformT . liftTransformT

refScopeBuilder :: RefNotation (ScopeBuilder a) -> ScopeBuilder a
refScopeBuilder = execMapTransformT

pureScopeBuilder :: QScope -> ScopeBuilder ()
pureScopeBuilder scope = interpScopeBuilder $ registerScope scope

allocateVarScopeBuilder :: FullNameRef -> ScopeBuilder (FullName, VarID)
allocateVarScopeBuilder n = interpScopeBuilder $ allocateVar n
