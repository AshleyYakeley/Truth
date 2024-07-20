{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Action
    ( actionLibSection
    ) where

import Import
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Type
import Pinafore.Language.Var

qfail :: Text -> Action BottomType
qfail t = fail $ unpack t

orStop :: Maybe A -> Action A
orStop = actionLiftViewKnow . pure . maybeToKnow

onStop :: Action A -> Action A -> Action A
onStop p q = p <|> q

tryStop :: Action A -> Action (Maybe A)
tryStop action = actionLiftView $ fmap knowToMaybe $ unliftAction action

tryStop_ :: Action () -> Action ()
tryStop_ action = actionLiftView $ fmap (\_ -> ()) $ unliftAction action

actionLibSection :: LibraryStuff context
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
          , addNameInRootBDS $
            valBDS "orStop" "`orStop $ Just x` is `pure x`, while `orStop Nothing` is `stop`." $ orStop
          , addNameInRootBDS $ valBDS "onStop" "`onStop p q` does `p` first, and if it stops, then does `q`." $ onStop
          , addNameInRootBDS $
            valBDS
                "forever"
                "Run this action repeatedly forever. Use `stop` to break out, propagating the stop.  \nSame as `fn x => let rec fx = x >> fx in fx`." $
            (forever :: Action () -> Action BottomType)
          , addNameInRootBDS $
            valBDS
                "tryStop"
                "Run action. If it stops, catch and return `Nothing`.  \nSame as `fn x => onStop (map.Action Just x) $ pure Nothing`." $
            tryStop
          , addNameInRootBDS $
            valBDS "tryStop_" "Run action. If it stops, catch and return `()`.  \nSame as `fn x => onStop x $ pure ()`." $
            tryStop_
          , addNameInRootBDS $ valBDS "sleep" "Do nothing for this duration." threadSleep
          ]
        ]
