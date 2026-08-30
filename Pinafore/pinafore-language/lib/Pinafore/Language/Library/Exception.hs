{-# LANGUAGE ApplicativeDo #-}

{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Exception
    ( exceptionLibSection
    , TextException (..)
    , StopException (..)
    )
where

import System.IO.Error
import Text.Parsec.Pos

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Action ()
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Showable
import Pinafore.Language.Type

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
        in (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily TextException)|]) "TextException.")
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
        in (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily StopException)|]) "Stop.")
            { qgtGreatestDynamicSupertype = gds
            }

instance HasQGroundType '[CoCCRVariance] Located where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Located)|]) "Located."

toLocated :: Text -> Int -> Int -> A -> Located A
toLocated n r c i = MkLocated (newPos (unpack n) r c) toText i

fromLocated :: Located A -> (Text, (Int, (Int, (A, ()))))
fromLocated (MkLocated spos _ item) = (pack $ sourceName spos, (sourceLine spos, (sourceColumn spos, (item, ()))))

instance Exception (Located ActionException)

locatedToException :: Located ActionException -> ActionException
locatedToException = ExActionException . toException

locatedFromException :: ActionException -> Maybe (Located ActionException, ())
locatedFromException = \case
    ExActionException se -> do
        lae <- fromException se
        return (lae, ())
    _ -> Nothing

exceptionLibSection :: LibraryStuff
exceptionLibSection =
    headingBDS
        "Exceptions"
        ""
        [ typeBDS "Exception" "" (qSomeGroundType @_ @ActionException) []
        , showableSubtypeRelationEntry @ActionException "" showText
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
        , headingBDS
            "Located"
            ""
            [ typeBDS
                "Located"
                "Something located in textual source."
                (qSomeGroundType @_ @Located)
                [ valPatBDS
                    "Mk"
                    "Construct a `Located` from source, line, column, item."
                    toLocated
                    $ PureFunction
                    $ pure fromLocated
                , valPatBDS
                    "MkException"
                    "Construct an `Exception` from a `Located."
                    locatedToException
                    $ ImpureFunction
                    $ pure locatedFromException
                ]
            , hasSubtypeRelationBDS @(Located Showable) @Showable TrustMe ""
                $ functionToShim "toShowable"
                $ MkShowable
                . functionToShim "showText" showText
            , hasSubtypeRelationBDS @(Located ActionException) @ActionException Verify ""
                $ functionToShim "locatedToException" locatedToException
            ]
        ]
