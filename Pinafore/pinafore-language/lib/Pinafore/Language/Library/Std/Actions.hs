{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Std.Actions
    ( actionsLibEntries
    ) where

import Pinafore.Base
import Pinafore.Language.If
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Pinafore.Language.Type
import Pinafore.Language.Var
import Shapes

qfail :: Text -> Action BottomType
qfail t = fail $ unpack t

onStop :: Action A -> Action A -> Action A
onStop p q = p <|> q

actionsLibEntries :: [BindDocTree context]
actionsLibEntries =
    [ headingBDT
          "Actions"
          ""
          [ typeBDT "Action" "" (MkSomeGroundType actionGroundType) []
          , valBDT "return" "A value as an Action." $ return @Action @A
          , valBDT ">>=" "Bind the result of an Action to an Action." $ qbind
          , valBDT ">>" "Do actions in sequence." $ qbind_
          , valBDT "mapAction" "Map a function on an action." (fmap :: (A -> B) -> Action A -> Action B)
          , valBDT "fixAction" "The fixed point of an Action." $ mfix @Action @A
          , valBDT "fail" "Fail, causing the program to terminate with error." $ qfail
          , valBDT
                "stop"
                "Stop. This is similar to an exception that can be caught with `onStop`. The default handler (for the main program, button presses, etc.), is to catch and ignore it."
                (empty :: Action BottomType)
          , valBDT "onStop" "`onStop p q` does `p` first, and if it stops, then does `q`." $ onStop
          , valBDT "for_" "Perform an action on each value of a list." (for_ :: [A] -> (A -> Action ()) -> Action ())
          , valBDT
                "for"
                "Perform an action on each value of a list, returning a list."
                (for :: [A] -> (A -> Action B) -> Action [B])
          , valBDT "sleep" "Do nothing for this duration." threadSleep
          ]
    ]
