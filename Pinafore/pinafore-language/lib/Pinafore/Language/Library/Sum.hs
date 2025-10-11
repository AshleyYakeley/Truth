module Pinafore.Language.Library.Sum
    ( sumLibSection
    )
where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Type

sumLibSection :: LibraryStuff
sumLibSection =
    headingBDS
        "Type Sum"
        ""
        [ typeBDS "+:" "" (MkSomeGroundType eitherGroundType)
            $ fmap
                addNameInRootBDS
                [ valPatBDS "Left" "Construct an Either from the left." (Left @A @B)
                    $ ImpureFunction
                    $ pure
                    $ \(v :: Either A B) ->
                        case v of
                            Left a -> Just (a, ())
                            _ -> Nothing
                , valPatBDS "Right" "Construct an Either from the right." (Right @A @B)
                    $ ImpureFunction
                    $ pure
                    $ \(v :: Either A B) ->
                        case v of
                            Right a -> Just (a, ())
                            _ -> Nothing
                ]
        , hasSubtypeRelationBDS @(Either Entity Entity) @Entity Verify ""
            $ functionToShim "eitherEntityConvert" eitherEntityConvert
        , hasSubtypeRelationBDS @(Either Showable Showable) @Showable Verify "" $ functionToShim "show" textShowable
        , namespaceBDS "Sum"
            $ monadEntries @(Either P)
            <> [ valBDS "from" "Eliminate a sum" $ either @A @C @B
               , addNameInRootBDS
                    $ valBDS "either" "Eliminate a sum"
                    $ \(v :: Either A A) ->
                        case v of
                            Left a -> a
                            Right a -> a
               ]
        ]
