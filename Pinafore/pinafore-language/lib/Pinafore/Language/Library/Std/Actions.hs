{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Std.Actions
    ( actionsLibEntries
    ) where

import Pinafore.Base
import Pinafore.Context
import Pinafore.Language.DocTree
import Pinafore.Language.If
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Pinafore.Language.Type
import Pinafore.Language.Var
import Shapes

output :: (?pinafore :: PinaforeContext) => Text -> PinaforeAction ()
output text = liftIO $ hPutStrLn pinaforeStdOut $ unpack text

outputLn :: (?pinafore :: PinaforeContext) => Text -> PinaforeAction ()
outputLn text = liftIO $ hPutStrLn pinaforeStdOut $ unpack text

qfail :: Text -> PinaforeAction BottomType
qfail t = fail $ unpack t

onStop :: PinaforeAction A -> PinaforeAction A -> PinaforeAction A
onStop p q = p <|> q

actionsLibEntries :: [DocTreeEntry BindDoc]
actionsLibEntries =
    [ docTreeEntry
          "Actions"
          ""
          [ mkTypeEntry "Action" "" $ MkSomeGroundType actionGroundType
          , mkValEntry "return" "A value as an Action." $ return @PinaforeAction @A
          , mkValEntry ">>=" "Bind the result of an Action to an Action." $ qbind
          , mkValEntry ">>" "Do actions in sequence." $ qbind_
          , mkValEntry
                "mapAction"
                "Map a function on an action."
                (fmap :: (A -> B) -> PinaforeAction A -> PinaforeAction B)
          , mkValEntry "fixAction" "The fixed point of an Action." $ mfix @PinaforeAction @A
          , mkValEntry "fail" "Fail, causing the program to terminate with error." $ qfail
          , mkValEntry
                "stop"
                "Stop. This is similar to an exception that can be caught with `onStop`. The default handler (for the main program, button presses, etc.), is to catch and ignore it."
                (empty :: PinaforeAction BottomType)
          , mkValEntry "onStop" "`onStop p q` does `p` first, and if it stops, then does `q`." $ onStop
          , mkValEntry
                "for_"
                "Perform an action on each value of a list."
                (for_ :: [A] -> (A -> PinaforeAction ()) -> PinaforeAction ())
          , mkValEntry
                "for"
                "Perform an action on each value of a list, returning a list."
                (for :: [A] -> (A -> PinaforeAction B) -> PinaforeAction [B])
          , mkValEntry "output" "Output text to standard output." $ output
          , mkValEntry "outputLn" "Output text and a newline to standard output." $ outputLn
          , mkValEntry "sleep" "Do nothing for this duration." threadSleep
          ]
    ]
