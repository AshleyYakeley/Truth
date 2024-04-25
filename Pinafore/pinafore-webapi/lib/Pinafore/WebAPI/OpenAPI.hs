module Pinafore.WebAPI.OpenAPI
    ( openAPIImporter
    ) where

import Data.Aeson as Aeson hiding (Result)
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import Data.OpenApi hiding (items, name)
import Data.Shim
import Pinafore.Language
import Pinafore.Language.API
import Pinafore.WebAPI.Fetch
import Shapes hiding (Param)

pathItemOperations :: PathItem -> [(Text, Operation)]
pathItemOperations PathItem {..} = let
    getOp (opname, mop) = do
        op <- mop
        return (opname, op)
    in mapMaybe
           getOp
           [ ("GET", _pathItemGet)
           , ("PUT", _pathItemPut)
           , ("POST", _pathItemPost)
           , ("DELETE", _pathItemDelete)
           , ("OPTIONS", _pathItemOptions)
           , ("HEAD", _pathItemHead)
           , ("PATCH", _pathItemPatch)
           , ("TRACE", _pathItemTrace)
           ]

type M = Result Text

runM :: M a -> ResultT Text IO a
runM = liftInner

runMText :: M Text -> Text
runMText (SuccessResult t) = t
runMText (FailureResult err) = "<error: " <> err <> ">"

getReferenced :: Referenced a -> M a
getReferenced =
    \case
        Ref ref -> throwExc $ "missing reference " <> getReference ref
        Inline a -> return a

mangle :: Text -> Name
mangle = let
    m :: Char -> String
    m ' ' = "__"
    m '_' = "_U"
    m c = pure c
    in MkName . mconcat . fmap (pack . m) . unpack

operationToFunction :: Operation -> M (Name, [Param])
operationToFunction Operation {..} = do
    case _operationDeprecated of
        Just True -> throwExc "deprecated"
        _ -> return ()
    opid <- maybeToM "missing _operationOperationId" _operationOperationId
    params <- for _operationParameters getReferenced
    return (mangle opid, params)

showSchema :: Schema -> M Text
showSchema Schema {..}
    | Just t <- _schemaType =
        case t of
            OpenApiArray -> do
                items <- maybeToM "missing _schemaItems" _schemaItems
                case items of
                    OpenApiItemsObject rs -> do
                        itemschema <- getReferenced rs
                        itemtext <- showSchema itemschema
                        return $ "List " <> itemtext
                    OpenApiItemsArray rss -> do
                        sct <-
                            for rss $ \rs -> do
                                itemschema <- getReferenced rs
                                showSchema itemschema
                        return $ intercalate " *: " sct
            OpenApiString -> return "Text"
            OpenApiInteger -> return "Integer"
            _ -> return $ showText t
showSchema _ = throwExc "missing _schemaType"

showParam :: Param -> Text
showParam Param {..} = let
    sig =
        runMText $ do
            ref <- maybeToM "missing _paramSchema" _paramSchema
            sch <- getReferenced ref
            t <- showSchema sch
            return $ ": " <> t
    in _paramName <> sig

showOperation :: Operation -> M Text
showOperation op = do
    (opid, params) <- operationToFunction op
    return $ showText opid <> "(" <> intercalate ", " (fmap showParam params) <> ")"

data Func r where
    MkFunc :: QShimWit 'Positive t -> ((Object -> IO r) -> t) -> Func r

mkParam :: Param -> QShimWit 'Negative Value
mkParam _ = mapShimWit (MkPolarShim $ functionToShim "param" String) qType

mkFunc :: QShimWit 'Positive r -> [Param] -> Func r
mkFunc tr [] = MkFunc (actionShimWit tr) $ \call -> liftIO $ call mempty
mkFunc tr (p:pp) =
    case mkParam p of
        MkShimWit ta (MkPolarShim pf) ->
            case mkFunc tr pp of
                MkFunc tf f ->
                    MkFunc (funcShimWit (mkShimWit ta) tf) $ \call a ->
                        f $ \obj -> call $ Aeson.insert (Aeson.fromText $ _paramName p) (shimToFunction pf a) obj

importOpenAPI :: Text -> ResultT Text IO (LibraryStuff ())
importOpenAPI t = do
    bs <- fetch t
    jsonval <-
        case eitherDecode bs of
            Left err -> liftInner $ FailureResult $ "invalid JSON: " <> pack err
            Right jsonval -> return jsonval
    root :: OpenApi <-
        case fromJSON jsonval of
            Error err -> liftInner $ FailureResult $ pack err
            Success val -> return val
    let
        operations :: [(Operation, Text, Text)]
        operations = do
            (path, pathitem) <- InsOrd.toList $ _openApiPaths root
            (opname, op) <- pathItemOperations pathitem
            return (op, opname, pack path)
        mkOperationFunction :: (Operation, Text, Text) -> M (LibraryStuff ())
        mkOperationFunction (op, opname, path) = do
            (name, params) <- operationToFunction op
            let
                func :: Func Text
                func = mkFunc qType params
                call :: Object -> IO Text
                call _ = return $ opname <> " " <> path
            return $
                case func of
                    MkFunc qt f ->
                        valWitBDS
                            (UnqualifiedFullNameRef name)
                            (MkRawMarkdown $ fromMaybe "" $ _operationDescription op)
                            qt $
                        f call
    functions <- runM $ for operations mkOperationFunction
    return $
        mconcat $
        [ namespaceBDS
              "Info"
              [ valBDS "schemas" "" $ fmap showText $ InsOrd.toList $ _componentsSchemas $ _openApiComponents root
              , valBDS "servers" "" $ fmap showText $ _openApiServers root
              , valBDS "paths" "" $ fmap showText $ InsOrd.toList $ _openApiPaths root
              , valBDS "functions" "" $ do
                    (op, opname, path) <- operations
                    return $ runMText (showOperation op) <> " = " <> opname <> " " <> path
              ]
        ] <>
        functions

openAPIImporter :: Importer
openAPIImporter = MkImporter "openapi" importOpenAPI
