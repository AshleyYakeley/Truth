module Pinafore.Language.Library.Entity.Open
    ( openEntityLibSection
    ) where

import Import
import Pinafore.Language.Convert.Pinafore
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Types
import Pinafore.Language.Type

openEntityLibSection :: LibraryStuff
openEntityLibSection =
    headingBDS
        "Open Entities"
        ""
        [ namespaceBDS
              "OpenEntity"
              [ valBDS "point" "!{point @A <anchor>}: A\nAn open entity for this anchor. `A` is an open entity type." $ \(MkLangType t) anchor -> do
                    mtp <- getOpenEntityType t
                    return $
                        MkLangExpression $
                        case mtp of
                            MkSome (tp :: OpenEntityType tid) -> let
                                typef = openEntityShimWit tp
                                pt :: OpenEntity tid
                                pt = MkOpenEntity $ MkEntity anchor
                                in constSealedExpression $ MkSomeOf typef pt
              , valBDS "new" "!{new @A}: Action A\nGenerate an open entity. `A` is an open entity type." $ \(MkLangType t) -> do
                    mtp <- getOpenEntityType t
                    return $
                        MkLangExpression $
                        case mtp of
                            MkSome (tp :: OpenEntityType tid) -> let
                                pt :: Action (OpenEntity tid)
                                pt = liftIO $ newKeyContainerItem @(FiniteSet (OpenEntity tid))
                                typef = actionShimWit $ openEntityShimWit tp
                                in constSealedExpression $ MkSomeOf typef pt
              ]
        ]
