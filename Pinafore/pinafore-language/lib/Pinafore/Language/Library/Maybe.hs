module Pinafore.Language.Library.Maybe
    ( maybeLibSection
    ) where

import Import
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Type
import Pinafore.Language.Var

maybeLibSection :: LibraryStuff context
maybeLibSection =
    headingBDS
        "Maybe"
        ""
        [ typeBDS "Maybe" "" (MkSomeGroundType maybeGroundType) $
          fmap
              addNameInRootBDS
              [ valPatBDS "Just" "Construct a Maybe from a value." (Just @A) $
                ImpureFunction $ \(v :: Maybe A) ->
                    case v of
                        Just a -> Just (a, ())
                        _ -> Nothing
              , valPatBDS "Nothing" "Construct a Maybe without a value." (Nothing @BottomType) $
                ImpureFunction $ \(v :: Maybe A) ->
                    case v of
                        Nothing -> Just ()
                        _ -> Nothing
              ]
        , hasSubtypeRelationBDS @(Maybe Entity) @Entity Verify "" $
          functionToShim "maybeEntityConvert" maybeEntityConvert
        , hasSubtypeRelationBDS @(Maybe Showable) @Showable Verify "" $ functionToShim "show" textShowable
        , namespaceBDS "Maybe" $ monadEntries @_ @Maybe <> [valBDS "from" "" (fromMaybe :: A -> Maybe A -> A)]
        ]
