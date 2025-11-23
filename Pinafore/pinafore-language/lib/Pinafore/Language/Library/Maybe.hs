module Pinafore.Language.Library.Maybe
    ( maybeLibSection
    )
where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule

maybeLibSection :: LibraryStuff
maybeLibSection =
    headingBDS
        "Maybe"
        ""
        [ typeBDS_ @_ @LangMaybe "Maybe" ""
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
        , hasBiNeutralSubtypeRelationBDS @(LangMaybe A) @(Link A ())
        , namespaceBDS "Maybe"
            $ monadEntries @Maybe
            <> [ valBDS "from" "" (fromMaybe :: A -> Maybe A -> A)
               , valBDS "toList" "" (toList :: Maybe A -> [A])
               ]
        ]
