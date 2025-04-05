module Pinafore.Language.Library.Maybe
    ( maybeLibSection
    )
where

import Import
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Type
import Pinafore.Language.Var

maybeLibSection :: LibraryStuff
maybeLibSection =
    headingBDS
        "Maybe"
        ""
        [ typeBDS "Maybe" "" (MkSomeGroundType maybeGroundType)
            $ fmap
                addNameInRootBDS
                [ valPatBDS "Just" "Construct a Maybe from a value." (Just @A)
                    $ ImpureFunction
                    $ pure
                    $ \(v :: Maybe A) ->
                        case v of
                            Just a -> Just (a, ())
                            _ -> Nothing
                , valPatBDS "Nothing" "Construct a Maybe without a value." (Nothing @BottomType)
                    $ ImpureFunction
                    $ pure
                    $ \(v :: Maybe A) ->
                        case v of
                            Nothing -> Just ()
                            _ -> Nothing
                ]
        , hasSubtypeRelationBDS @(Maybe Entity) @Entity Verify ""
            $ functionToShim "maybeEntityConvert" maybeEntityConvert
        , hasSubtypeRelationBDS @(Maybe Showable) @Showable Verify "" $ functionToShim "show" textShowable
        , namespaceBDS "Maybe"
            $ monadEntries @Maybe
            <> [ valBDS "from" "" (fromMaybe :: A -> Maybe A -> A)
               , valBDS "toList" "" (toList :: Maybe A -> [A])
               ]
        ]
