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

exposeDeclids :: [Name] -> [DefDoc] -> [DefDoc]
exposeDeclids names decls = let
    inDecl :: Name -> Maybe DefDoc
    inDecl n = find (diMatchName n . docItem) decls
    isSubtypeDDI :: DefDoc -> Bool
    isSubtypeDDI doc =
        case docItem doc of
            SubtypeRelationDocItem {} -> True
            _ -> False
    in mapMaybe inDecl names <> filter isSubtypeDDI decls

exposeDocs :: [Name] -> Docs -> Docs
exposeDocs names = fmap EntryDocTreeEntry . exposeDeclids names . mconcat . fmap toList

type ScopeBuilder = TransformT RefNotation

runScopeBuilder :: ScopeBuilder a -> (a -> RefNotation b) -> RefNotation b
runScopeBuilder sb = runTransformT sb

sourcePosScopeBuilder :: SourcePos -> ScopeBuilder ()
sourcePosScopeBuilder = refPut $ transformParamRef $ liftParam $ liftParam sourcePosParam

interpScopeBuilder :: PinaforeScopeInterpreter --> ScopeBuilder
interpScopeBuilder = liftTransformT . liftTransformT

refScopeBuilder :: RefNotation (ScopeBuilder a) -> ScopeBuilder a
refScopeBuilder = execMapTransformT

pureScopeBuilder :: PinaforeScope -> ScopeBuilder ()
pureScopeBuilder scope = interpScopeBuilder (registerScope scope)

allocateVarScopeBuilder :: Name -> ScopeBuilder VarID
allocateVarScopeBuilder n = interpScopeBuilder $ allocateVar n
