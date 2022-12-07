{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Std.Actions
    ( actionsLibEntries
    ) where

import Pinafore.Base
import Pinafore.Language.DocTree
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

actionsLibEntries :: [DocTreeEntry (BindDocTree context)]
actionsLibEntries =
    [ docTreeEntry
          "Actions"
          ""
          [ mkTypeEntry "Action" "" (MkSomeGroundType actionGroundType) []
          , mkValEntry "return" "A value as an Action." $ return @Action @A
          , mkValEntry ">>=" "Bind the result of an Action to an Action." $ qbind
          , mkValEntry ">>" "Do actions in sequence." $ qbind_
          , mkValEntry "mapAction" "Map a function on an action." (fmap :: (A -> B) -> Action A -> Action B)
          , mkValEntry "fixAction" "The fixed point of an Action." $ mfix @Action @A
          , mkValEntry "fail" "Fail, causing the program to terminate with error." $ qfail
          , mkValEntry
                "stop"
                "Stop. This is similar to an exception that can be caught with `onStop`. The default handler (for the main program, button presses, etc.), is to catch and ignore it."
                (empty :: Action BottomType)
          , mkValEntry "onStop" "`onStop p q` does `p` first, and if it stops, then does `q`." $ onStop
          , mkValEntry
                "for_"
                "Perform an action on each value of a list."
                (for_ :: [A] -> (A -> Action ()) -> Action ())
          , mkValEntry
                "for"
                "Perform an action on each value of a list, returning a list."
                (for :: [A] -> (A -> Action B) -> Action [B])
          , mkValEntry "sleep" "Do nothing for this duration." threadSleep
          ]
    ]
