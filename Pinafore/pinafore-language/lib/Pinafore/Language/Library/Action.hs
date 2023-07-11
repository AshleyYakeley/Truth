{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Action
    ( actionLibSection
    ) where

import Pinafore.Base
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Type
import Pinafore.Language.Var
import Shapes

qfail :: Text -> Action BottomType
qfail t = fail $ unpack t

onStop :: Action A -> Action A -> Action A
onStop p q = p <|> q

actionLibSection :: BindDocStuff context
actionLibSection =
    headingBDS
        "Action"
        ""
        [ typeBDS "Action" "" (MkSomeGroundType actionGroundType) []
        , namespaceBDS "Action" $
          fmap addNameInRootBDS (monadEntries @_ @Action) <>
          [ addNameInRootBDS $ valBDS "fix" "The fixed point of an Action." $ mfix @Action @A
          , addNameInRootBDS $ valBDS "fail" "Fail, causing the program to terminate with error." $ qfail
          , addNameInRootBDS $
            valBDS
                "stop"
                "Stop. This is similar to an exception that can be caught with `onStop`. The default handler (for the main program, button presses, etc.), is to catch and ignore it."
                (empty :: Action BottomType)
          , addNameInRootBDS $ valBDS "onStop" "`onStop p q` does `p` first, and if it stops, then does `q`." $ onStop
          , addNameInRootBDS $
            valBDS "for_" "Perform an action on each value of a list." (for_ :: [A] -> (A -> Action ()) -> Action ())
          , addNameInRootBDS $
            valBDS
                "for"
                "Perform an action on each value of a list, returning a list."
                (for :: [A] -> (A -> Action B) -> Action [B])
          , addNameInRootBDS $ valBDS "sleep" "Do nothing for this duration." threadSleep
          ]
        ]
