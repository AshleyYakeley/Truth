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

tryStop :: Action A -> Action (Maybe A)
tryStop action = actionLiftView $ fmap knowToMaybe $ unliftAction action

tryStop_ :: Action () -> Action ()
tryStop_ action = actionLiftView $ fmap (\_ -> ()) $ unliftAction action

actionLibSection :: BindDocStuff context
actionLibSection =
    headingBDS
        "Action"
        ""
        [ typeBDS "Action" "" (MkSomeGroundType actionGroundType) []
        , namespaceBDS "Action" $
          fmap addNameInRootBDS (monadEntries @_ @Action) <>
          [ addNameInRootBDS $ valBDS "mfix" "The fixed point of an Action." $ mfix @Action @A
          , addNameInRootBDS $ valBDS "fail" "Fail, causing the program to terminate with error." $ qfail
          , addNameInRootBDS $
            valBDS
                "stop"
                "Stop. This is similar to an exception that can be caught with `onStop`. The default handler (for the main program, button presses, etc.), is to catch and ignore it."
                (empty :: Action BottomType)
          , addNameInRootBDS $ valBDS "onStop" "`onStop p q` does `p` first, and if it stops, then does `q`." $ onStop
          , addNameInRootBDS $
            valBDS
                "forever"
                "Run this action repeatedly forever. Use `stop` to break out, propagating the stop. Same as `fn x => let rec fx = x >> fx in fx`." $
            (forever :: Action () -> Action BottomType)
          , addNameInRootBDS $
            valBDS
                "tryStop"
                "Run action. If it stops, catch and return `Nothing`. Same as `fn x => onStop (map.Action Just x) $ pure Nothing`." $
            tryStop
          , addNameInRootBDS $
            valBDS "tryStop_" "Run action. If it stops, catch and return `()`. Same as `fn x => onStop x $ pure ()`." $
            tryStop_
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
