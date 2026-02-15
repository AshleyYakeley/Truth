module Pinafore.Language.Library.Entity.Boolean
    ( booleanEntityLibSection
    )
where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Entity.Literal
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Showable

showTrueFalse :: Bool -> Text
showTrueFalse = bool "false" "true"

booleanEntityLibSection :: LibraryStuff
booleanEntityLibSection =
    headingBDS
        "Boolean"
        ""
        [ typeBDS "Boolean" "" (MkSomeGroundType booleanGroundType)
            $ fmap
                addNameInRootBDS
                [ valPatBDS "True" "Boolean TRUE." True
                    $ ImpureFunction
                    $ pure
                    $ \v ->
                        if v
                            then Just ()
                            else Nothing
                , valPatBDS "False" "Boolean FALSE." False
                    $ ImpureFunction
                    $ pure
                    $ \v ->
                        if v
                            then Nothing
                            else Just ()
                ]
        , literalSubtypeRelationEntry @Bool
        , showableSubtypeRelationEntry "show as \"true\" or \"false\"" showTrueFalse
        , namespaceBDS "Boolean"
            $ eqEntries @Bool
            <> [ addNameInRootBDS $ valBDS "&&" "Boolean AND." (&&)
               , addNameInRootBDS $ valBDS "||" "Boolean OR." (||)
               , addNameInRootBDS $ valBDS "not" "Boolean NOT." not
               , addNameInRootBDS $ valBDS "showTrueFalse" "show as \"true\" or \"false\"" showTrueFalse
               , addNameInRootBDS $ valBDS "showYesNo" "show as \"yes\" or \"no\"" $ bool "no" ("yes" :: Text)
               ]
        ]
