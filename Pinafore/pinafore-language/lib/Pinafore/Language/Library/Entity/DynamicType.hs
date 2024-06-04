{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Entity.DynamicType
    ( dynamicTypeEntityLibSection
    ) where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type

-- ConcreteDynamicType
dynamicTypeStorableGroundType :: QGroundType '[] ConcreteDynamicType
dynamicTypeStorableGroundType =
    stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily ConcreteDynamicType)|]) "DynamicType"

instance HasQGroundType '[] ConcreteDynamicType where
    qGroundType = dynamicTypeStorableGroundType

dynamicTypeEntityLibSection :: LibraryStuff context
dynamicTypeEntityLibSection =
    headingBDS
        "Dynamic Types"
        ""
        [ typeBDS "DynamicType" "" (MkSomeGroundType dynamicTypeStorableGroundType) []
        , hasSubtypeRelationBDS @ConcreteDynamicType @Entity Verify "" $
          functionToShim "unConcreteDynamicType" $ \(MkConcreteDynamicType entity) -> entity
        , namespaceBDS
              "DynamicType"
              [ valBDS "fromEntity" "The type of this dynamic entity." $ \(MkDynamicEntity dt _) -> dt
              , valBDS "newEntity" "Generate a dynamic entity of this type." $ \dt -> do
                    e <- newKeyContainerItem @(FiniteSet Entity)
                    return $ MkDynamicEntity dt e
              , specialFormBDS
                    "fromType"
                    "The dynamic type of `A`, a concrete dynamic entity type."
                    ["@A"]
                    "DynamicType" $
                MkQSpecialForm (ConsListType AnnotPositiveType NilListType) $ \(t, ()) -> do
                    (_, dt) <- getConcreteDynamicEntityType t
                    return $ jmToValue dt
              , specialFormBDS
                    "fromAbstract"
                    "The dynamic types of `A`, a dynamic entity type."
                    ["@A"]
                    "List DynamicType" $
                MkQSpecialForm (ConsListType AnnotPositiveType NilListType) $ \(t, ()) -> do
                    dts <- getDynamicEntityType t
                    return $ jmToValue $ toList dts
              , specialFormBDS
                    "isSubtype"
                    "Whether this type is in `A`, a dynamic entity type."
                    ["@A"]
                    "DynamicType -> Boolean" $
                MkQSpecialForm (ConsListType AnnotPositiveType NilListType) $ \(t, ()) -> do
                    dts <- getDynamicEntityType t
                    return $ jmToValue $ \a -> member a dts
              ]
        ]
