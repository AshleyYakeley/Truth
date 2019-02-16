module Pinafore.Language.TypeID
    ( TypeID
    , zeroTypeID
    , succTypeID
    , typeIdToSymbolType
    ) where

import Shapes

newtype TypeID =
    MkTypeID Integer
    deriving (Eq)

zeroTypeID :: TypeID
zeroTypeID = MkTypeID 0

succTypeID :: TypeID -> TypeID
succTypeID (MkTypeID n) = MkTypeID $ succ n

typeIdToSymbolType :: TypeID -> (forall sym. SymbolType sym -> r) -> r
typeIdToSymbolType (MkTypeID n) = toSymbolType (show n)
