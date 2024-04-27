module Pinafore.WebAPI.JSONType where

import Pinafore.Language.API
import Shapes

data JSONType
    = NullJSONType
    | BoolJSONType
    | IntegerJSONType
    | StringJSONType
    | ListArrayJSONType JSONType
    | TupleArrayJSONType [JSONType]
    | ObjectJSONType [JSONParamType]

instance Show JSONType where
    show NullJSONType = "null"
    show BoolJSONType = "bool"
    show IntegerJSONType = "integer"
    show StringJSONType = "string"
    show (ListArrayJSONType t) = "(list " <> show t <> ")"
    show (TupleArrayJSONType tt) = "(tuple " <> intercalate "," (fmap show tt) <> ")"
    show (ObjectJSONType tt) = "{" <> intercalate "," (fmap show tt) <> "}"

data JSONParamType =
    MkJSONParamType Text
                    JSONOptType

instance Show JSONParamType where
    show (MkJSONParamType k v) = unpack k <> ":" <> show v

data Optionality
    = Required
    | Optional

data JSONOptType =
    MkJSONOptType Optionality
                  JSONType

instance Show JSONOptType where
    show (MkJSONOptType Required t) = show t
    show (MkJSONOptType Optional t) = show t <> "*"

class Intersectable t where
    intersectType :: t -> t -> Result Text t

intersectTypeAll :: Intersectable t => NonEmpty t -> Result Text t
intersectTypeAll (a :| []) = return a
intersectTypeAll (a :| b:cc) = do
    ab <- intersectType a b
    intersectTypeAll $ ab :| cc

instance Intersectable Optionality where
    intersectType Optional Optional = return Optional
    intersectType _ _ = return Required

instance Intersectable JSONOptType where
    intersectType (MkJSONOptType oa ta) (MkJSONOptType ob tb) = do
        oc <- intersectType oa ob
        tc <- intersectType ta tb
        return $ MkJSONOptType oc tc

instance Intersectable [JSONType] where
    intersectType [] [] = return []
    intersectType (a:aa) (b:bb) = do
        c <- intersectType a b
        cc <- intersectType aa bb
        return $ c : cc
    intersectType aa [] = throwExc $ "tuple extra: " <> showText aa
    intersectType [] bb = throwExc $ "tuple extra: " <> showText bb

lookupParam :: Text -> [JSONParamType] -> Maybe (JSONOptType, [JSONParamType])
lookupParam _ [] = Nothing
lookupParam key (MkJSONParamType k ta:pa)
    | k == key = Just (ta, pa)
lookupParam key (p:pa) = do
    (t, pb) <- lookupParam key pa
    return (t, p : pb)

instance Intersectable [JSONParamType] where
    intersectType [] pb = return $ pb
    intersectType pa [] = return $ pa
    intersectType (p@(MkJSONParamType k ta):pa) pb =
        case lookupParam k pb of
            Nothing -> do
                pc <- intersectType pa pb
                return $ p : pc
            Just (tb, pb') -> do
                tc <- intersectType ta tb
                pc <- intersectType pa pb'
                return $ MkJSONParamType k tc : pc

instance Intersectable JSONType where
    intersectType NullJSONType NullJSONType = return NullJSONType
    intersectType BoolJSONType BoolJSONType = return BoolJSONType
    intersectType IntegerJSONType IntegerJSONType = return IntegerJSONType
    intersectType (ListArrayJSONType ta) (ListArrayJSONType tb) = do
        tc <- intersectType ta tb
        return $ ListArrayJSONType tc
    intersectType (TupleArrayJSONType lta) (TupleArrayJSONType ltb) = do
        ltc <- intersectType lta ltb
        return $ TupleArrayJSONType ltc
    intersectType (ObjectJSONType pa) (ObjectJSONType pb) = do
        pc <- intersectType pa pb
        return $ ObjectJSONType pc
    intersectType ta tb = throwExc $ "incompatible: " <> showText ta <> " and " <> showText tb
