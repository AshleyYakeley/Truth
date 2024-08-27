module Pinafore.Language.Library.Entity.Open
    ( openEntityLibSection
    ) where

import Import
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Types
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type

openEntityLibSection :: LibraryStuff
openEntityLibSection =
    headingBDS
        "Open Entities"
        ""
        [ namespaceBDS
              "OpenEntity"
              [ specialFormBDS
                    "point"
                    "An open entity for this anchor. `A` is an open entity type."
                    ["@A", "<anchor>"]
                    "A" $
                MkQSpecialForm (ConsListType AnnotPositiveType $ ConsListType AnnotAnchor NilListType) $ \(t, (anchor, ())) -> do
                    mtp <- getOpenEntityType t
                    return $
                        case mtp of
                            MkSome (tp :: OpenEntityType tid) -> let
                                typef = openEntityShimWit tp
                                pt :: OpenEntity tid
                                pt = MkOpenEntity $ MkEntity anchor
                                in constSealedExpression $ MkSomeOf typef pt
              , specialFormBDS "new" "Generate an open entity. `A` is an open entity type." ["@A"] "Action A" $
                MkQSpecialForm (ConsListType AnnotPositiveType NilListType) $ \(t, ()) -> do
                    mtp <- getOpenEntityType t
                    return $
                        case mtp of
                            MkSome (tp :: OpenEntityType tid) -> let
                                pt :: Action (OpenEntity tid)
                                pt = liftIO $ newKeyContainerItem @(FiniteSet (OpenEntity tid))
                                typef = actionShimWit $ openEntityShimWit tp
                                in constSealedExpression $ MkSomeOf typef pt
              ]
        ]
