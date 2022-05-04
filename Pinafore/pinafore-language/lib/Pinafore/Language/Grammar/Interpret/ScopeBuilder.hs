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
    inDecl n = find (\doc -> docName doc == toText n) decls
    isSubtypeDDI :: DefDoc -> Bool
    isSubtypeDDI doc =
        case docType doc of
            SubtypeRelationDocType -> True
            _ -> False
    in mapMaybe inDecl names <> filter isSubtypeDDI decls

exposeDocs :: [Name] -> Docs -> Docs
exposeDocs names = fmap EntryDocTreeEntry . exposeDeclids names . mconcat . fmap toList

type ScopeBuilder = TransformT RefNotation

runScopeBuilder :: ScopeBuilder a -> (a -> RefNotation b) -> RefNotation b
runScopeBuilder sb = runTransformT sb

sourcePosScopeBuilder :: SourcePos -> ScopeBuilder ()
sourcePosScopeBuilder = refPut $ transformParamRef $ liftParam $ liftParam sourcePosParam

interpScopeBuilder :: (PinaforeInterpreter --> PinaforeInterpreter) -> ScopeBuilder ()
interpScopeBuilder mf = mapTransformT $ hoistRefNotation $ MkWMFunction mf

refScopeBuilder :: RefNotation (ScopeBuilder a) -> ScopeBuilder a
refScopeBuilder = execMapTransformT

pureScopeBuilder :: PinaforeScope -> ScopeBuilder ()
pureScopeBuilder scope = interpScopeBuilder (importScope scope)

allocateVarScopeBuilder :: Name -> ScopeBuilder VarID
allocateVarScopeBuilder n = liftTransformT $ liftTransformT $ MkTransformT $ allocateVar n
