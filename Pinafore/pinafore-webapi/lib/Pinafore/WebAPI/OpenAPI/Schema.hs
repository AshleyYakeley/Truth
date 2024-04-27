module Pinafore.WebAPI.OpenAPI.Schema where

import qualified Data.HashMap.Strict.InsOrd as InsOrd
import Data.OpenApi hiding (items, name, schema)
import Pinafore.Language.API
import Pinafore.WebAPI.JSONType
import qualified Shapes
import Shapes hiding (Param)

type M = ReaderT (Components, [Reference]) (Result Text)

componentsParam :: Shapes.Param M Components
componentsParam = lensMapParam (lensToVL fstLens) readerParam

stackParam :: Shapes.Param M [Reference]
stackParam = lensMapParam (lensToVL sndLens) readerParam

runM :: Components -> M a -> ResultT Text IO a
runM c ma = liftInner $ runReaderT ma (c, [])

getReferenced :: (Components -> Definitions a) -> Referenced a -> M a
getReferenced _ (Inline a) = return a
getReferenced sel (Ref ref) = do
    comps <- paramAsk componentsParam
    case InsOrd.lookup (getReference ref) $ sel comps of
        Just a -> return a
        Nothing -> throwExc $ "missing reference: " <> getReference ref

withReferenced :: (Components -> Definitions a) -> Referenced a -> (a -> M b) -> M b
withReferenced _ (Inline a) call = call a
withReferenced sel (Ref ref) call = do
    stack <- paramAsk stackParam
    if elem ref stack
        then throwExc $ "reference loop: " <> intercalate ", " (fmap getReference stack)
        else do
            comps <- paramAsk componentsParam
            case InsOrd.lookup (getReference ref) $ sel comps of
                Just a -> paramLocal stackParam (\s -> ref : s) $ call a
                Nothing -> throwExc $ "missing reference: " <> getReference ref

schemaToJSONType :: Schema -> M JSONType
schemaToJSONType schema =
    case _schemaType schema of
        Just t ->
            case t of
                OpenApiArray -> do
                    items <- maybeToM "missing _schemaItems" $ _schemaItems schema
                    case items of
                        OpenApiItemsObject rs -> do
                            itemp <- refSchemaToJSONType rs
                            return $ ListArrayJSONType itemp
                        OpenApiItemsArray rss -> do
                            pp <- for rss $ \rs -> refSchemaToJSONType rs
                            return $ TupleArrayJSONType pp
                OpenApiString -> return StringJSONType
                OpenApiInteger -> return IntegerJSONType
                OpenApiBoolean -> return BoolJSONType
                OpenApiNull -> return NullJSONType
                OpenApiObject -> do
                    props <-
                        for (InsOrd.toList $ _schemaProperties schema) $ \(k, rs) -> do
                            pt <- refSchemaToJSONType rs
                            return $ MkJSONParamType k $ MkJSONOptType Required pt
                    return $ ObjectJSONType $ props
                OpenApiNumber -> throwExc $ "unsupported _schemaType: " <> showText t
        Nothing ->
            case _schemaAllOf schema of
                Just (a:aa) -> do
                    tt <- for (a :| aa) refSchemaToJSONType
                    lift $ intersectTypeAll tt
                _ -> throwExc $ "unsupported _schema: " <> showText schema

refSchemaToJSONType :: Referenced Schema -> M JSONType
refSchemaToJSONType rs = withReferenced _componentsSchemas rs schemaToJSONType
