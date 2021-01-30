module Pinafore.Language.Library
    ( DefDoc(..)
    , DocTree
    , runDocTree
    , libraryDoc
    , LibraryContext(..)
    , mkLibraryContext
    , allOperatorNames
    ) where

import Pinafore.Context
import Pinafore.Language.DocTree
import Pinafore.Language.Error
import Pinafore.Language.Grammar
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Debug
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std
import Pinafore.Language.Library.UI
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes

library :: [LibraryModule]
library = [stdLibraryModule, uiLibraryModule, debugLibraryModule]

getModuleScope :: (?pinafore :: PinaforeContext) => LibraryModule -> IO PinaforeScope
getModuleScope dt = do
    let
        bindDocs :: [BindDoc]
        bindDocs = toList dt
        seBinding :: ScopeEntry -> Maybe (Name, PinaforeBinding)
        seBinding (BindScopeEntry name mb) = do
            b <- mb
            return (name, b ?pinafore)
        seBinding _ = Nothing
        bscope :: PinaforeScope
        bscope = bindingsScope $ mapFromList $ mapMaybe (seBinding . bdScopeEntry) bindDocs
        seSubtype :: ScopeEntry -> [SubypeConversionEntry PinaforeGroundType]
        seSubtype (SubtypeScopeEntry entries) = entries
        seSubtype _ = []
    sscope <- getSubtypesScope $ mconcat $ fmap (seSubtype . bdScopeEntry) bindDocs
    return $ bscope <> sscope

libraryDoc :: [DocTree DefDoc]
libraryDoc = fmap (fmap bdDoc) library

nameIsInfix :: Name -> Bool
nameIsInfix n =
    case unpack n of
        (c:_)
            | isAlpha c -> False
        "[]" -> False
        _ -> True

allOperatorNames :: [Name]
allOperatorNames = let
    getDocName MkDefDoc {docType = ValueDocType, ..}
        | nameIsInfix (MkName docName) = Just $ MkName docName
    getDocName _ = Nothing
    in catMaybes $ fmap getDocName $ mconcat $ fmap toList libraryDoc

getImplictScope :: (?pinafore :: PinaforeContext) => IO PinaforeScope
getImplictScope = getModuleScope stdLibraryModule

getLibraryModuleMap :: (?pinafore :: PinaforeContext) => IO (Map Text PinaforeScope)
getLibraryModuleMap = do
    pairs <-
        for library $ \lmod -> do
            scope <- getModuleScope lmod
            return (docTreeName lmod, scope)
    return $ mapFromList pairs

getLibraryScope :: (?pinafore :: PinaforeContext) => IO (ModuleName -> Maybe PinaforeScope)
getLibraryScope = do
    libraryModuleMap <- getLibraryModuleMap
    return $ \mname -> lookup (toText mname) libraryModuleMap

data LibraryContext = MkLibraryContext
    { lcImplictScope :: PinaforeScope
    , lcLoadModule :: ModuleName -> PinaforeInterpreter (Maybe PinaforeScope)
    }

loadModule ::
       PinaforeScope
    -> (ModuleName -> Maybe PinaforeScope)
    -> FetchModule
    -> ModuleName
    -> PinaforeInterpreter (Maybe PinaforeScope)
loadModule _ libLookup _ mname
    | Just scope <- libLookup mname = return $ Just scope
loadModule implictScope _ fetchModule mname = do
    mrr <- liftIO $ runFetchModule fetchModule mname
    case mrr of
        Nothing -> return Nothing
        Just (fpath, FailureResult err) ->
            throw $ MkErrorMessage (initialPos fpath) $ UnicodeDecodeError $ pack $ show err
        Just (fpath, SuccessResult text) ->
            fmap Just $ importScope implictScope $ runSourcePos (initialPos fpath) $ parseModule text

mkLibraryContext :: (?pinafore :: PinaforeContext) => FetchModule -> IO LibraryContext
mkLibraryContext fetchModule = do
    lcImplictScope <- getImplictScope
    libLookup <- getLibraryScope
    let lcLoadModule = loadModule lcImplictScope libLookup fetchModule
    return MkLibraryContext {..}
