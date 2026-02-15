{-# LANGUAGE ApplicativeDo #-}

{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Action
    ( actionLibSection
    , TextException (..)
    , StopException (..)
    )
where

import System.IO.Error

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Type

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

instance HasQGroundType '[] ActionException where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily ActionException)|]) "Exception"

newtype TextException = MkTextException {unTextException :: Text}

instance HasQGroundType '[] TextException where
    qGroundType = let
        gds :: QPolyGreatestDynamicSupertype '[] TextException
        gds =
            varPolyGreatestDynamicSupertype
                NilCCRArguments
                $ mapNegShimWit
                    ( functionToShim "" $ \case
                        ExActionException se | Just e <- fromException se, isUserError e -> Just $ MkTextException $ pack $ ioeGetErrorString e
                        _ -> Nothing
                    )
                    (qGroundedType :: _ ActionException)
        in (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily TextException)|]) "TextException")
            { qgtGreatestDynamicSupertype = gds
            }

data StopException = MkStopException

instance HasQGroundType '[] StopException where
    qGroundType = let
        gds :: QPolyGreatestDynamicSupertype '[] StopException
        gds =
            varPolyGreatestDynamicSupertype
                NilCCRArguments
                $ mapNegShimWit
                    ( functionToShim "" $ \case
                        StopActionException -> Just MkStopException
                        _ -> Nothing
                    )
                    (qGroundedType :: _ ActionException)
        in (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily StopException)|]) "Stop")
            { qgtGreatestDynamicSupertype = gds
            }

actionLibSection :: LibraryStuff
actionLibSection =
    headingBDS
        "Action"
        ""
        [ typeBDS "Action" "" (qSomeGroundType @_ @Action) []
        , namespaceBDS "Action"
            $ fmap addNameInRootBDS (monadExceptionEntries @Action)
            <> [ addNameInRootBDS $ valBDS "mfix" "The fixed point of an Action." $ mfix @Action @A
               , addNameInRootBDS $ valBDS "fail" "Fail, causing the program to terminate with error." $ qfail
               , addNameInRootBDS
                    $ valBDS
                        "stop"
                        "Stop. This is similar to an exception that can be caught with `onStop`. The default handler (for the main program, button presses, etc.), is to catch and ignore it."
                        (empty :: Action BottomType)
               , addNameInRootBDS
                    $ valBDS "orStop" "`orStop $ Just x` is `pure x`, while `orStop Nothing` is `stop`."
                    $ orStop
               , addNameInRootBDS $ valBDS "onStop" "`onStop p q` does `p` first, and if it stops, then does `q`." $ onStop
               , addNameInRootBDS
                    $ valBDS
                        "forever"
                        "Run this action repeatedly forever. Use `stop` to break out, propagating the stop.  \nSame as `fn x => let rec fx = x >> fx in fx`."
                    $ (forever :: Action () -> Action BottomType)
               , addNameInRootBDS
                    $ valBDS
                        "tryStop"
                        "Run action. If it stops, catch and return `Nothing`.  \nSame as `fn x => onStop (map.Action Just x) $ pure Nothing`."
                    $ tryStop
               , addNameInRootBDS
                    $ valBDS "tryStop_" "Run action. If it stops, catch and return `()`.  \nSame as `fn x => onStop x $ pure ()`."
                    $ tryStop_
               , addNameInRootBDS $ valBDS "sleep" "Do nothing for this duration." threadSleep
               ]
        , hasSubtypeRelationBDS @(Result ActionException A) @(Action A) Verify ""
            $ functionToShim "fromResultExc" fromResultExc
        , typeBDS
            "Stop"
            ""
            (qSomeGroundType @_ @StopException)
            [ valPatBDS "Mk" "" MkStopException $ PureFunction $ pure $ \MkStopException -> ()
            ]
        , hasSubtypeRelationBDS @StopException @ActionException Verify ""
            $ functionToShim "StopActionException"
            $ \MkStopException -> StopActionException
        , typeBDS "Exception" "" (qSomeGroundType @_ @ActionException) []
        , typeBDS
            "TextException"
            ""
            (qSomeGroundType @_ @TextException)
            [ valPatBDS "Mk" "" MkTextException $ PureFunction $ pure $ \(MkTextException t) -> (t, ())
            ]
        , hasSubtypeRelationBDS @TextException @ActionException Verify ""
            $ functionToShim "userError"
            $ ExActionException
            . toException
            . userError
            . unpack
            . unTextException
        ]
