module Pinafore.Language.Library.Entity.Dynamic
    ( dynamicEntityLibSection
    ) where

import Import
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Types
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type

dynamicEntityLibSection :: LibraryStuff context
dynamicEntityLibSection =
    headingBDS
        "Dynamic Entities"
        ""
        [ typeBDS "DynamicEntity" "" (MkSomeGroundType dynamicEntityStorableGroundType) []
        , hasSubtypeRelationBDS @DynamicEntity @Entity Verify "" $
          functionToShim "dynamicStoreAdapter" $ storeAdapterConvert $ dynamicStoreAdapter Nothing
        , namespaceBDS
              "DynamicEntity"
              [ specialFormBDS
                    "point"
                    "A dynamic entity for this anchor. `A` is a concrete dynamic entity type."
                    ["@A", "<anchor>"]
                    "A" $
                MkQSpecialForm (ConsListType AnnotPositiveType $ ConsListType AnnotAnchor NilListType) $ \(t, (anchor, ())) -> do
                    (n, dt) <- getConcreteDynamicEntityType t
                    let
                        typef = concreteDynamicEntityShimWit n dt
                        pt :: DynamicEntity
                        pt = MkDynamicEntity dt $ MkEntity anchor
                    return $ MkSomeOf typef pt
              , specialFormBDS
                    "new"
                    "Generate a dynamic entity. `A` is a concrete dynamic entity type."
                    ["@A"]
                    "Action A" $
                MkQSpecialForm (ConsListType AnnotPositiveType NilListType) $ \(t, ()) -> do
                    (n, dt) <- getConcreteDynamicEntityType t
                    let
                        pt :: Action DynamicEntity
                        pt =
                            liftIO $ do
                                e <- newKeyContainerItem @(FiniteSet Entity)
                                return $ MkDynamicEntity dt e
                        typef = actionShimWit $ concreteDynamicEntityShimWit n dt
                    return $ MkSomeOf typef pt
              ]
        ]
