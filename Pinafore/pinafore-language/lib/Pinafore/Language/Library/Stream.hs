{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Stream
    ( streamLibraryModule
    , LangSink(..)
    , LangSource(..)
    , langSinkWriteLn
    ) where

import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Pinafore.Language.Type
import Pinafore.Language.Var
import Shapes

-- ItemOrEnd
instance MaybeRepresentational ItemOrEnd where
    maybeRepresentational = Just Dict

instance HasVariance ItemOrEnd where
    type VarianceOf ItemOrEnd = 'Covariance

endOrItemGroundType :: QGroundType '[ CoCCRVariance] ItemOrEnd
endOrItemGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily ItemOrEnd)|]) "ItemOrEnd"

instance HasQGroundType '[ CoCCRVariance] ItemOrEnd where
    qGroundType = endOrItemGroundType

-- LangSink
newtype LangSink a =
    MkLangSink (Sink Action a)
    deriving (Contravariant)

instance MaybeRepresentational LangSink where
    maybeRepresentational = Nothing

instance HasVariance LangSink where
    type VarianceOf LangSink = 'Contravariance

toLangSink :: forall a. (ItemOrEnd a -> Action ()) -> LangSink a
toLangSink = MkLangSink . MkSink

fromLangSink :: forall a. LangSink a -> ItemOrEnd a -> Action ()
fromLangSink (MkLangSink (MkSink f)) = f

sinkGroundType :: QGroundType '[ ContraCCRVariance] LangSink
sinkGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangSink)|]) "Sink"

instance HasQGroundType '[ ContraCCRVariance] LangSink where
    qGroundType = sinkGroundType

liftSink :: Sink IO a -> LangSink a
liftSink sink = MkLangSink $ hoistSink liftIO sink

langSinkWrite :: forall a. LangSink a -> a -> Action ()
langSinkWrite (MkLangSink sink) = sinkWrite sink

langSinkWriteEnd :: forall a. LangSink a -> Action ()
langSinkWriteEnd (MkLangSink sink) = sinkWriteEnd sink

langSinkWriteLn :: LangSink Text -> Text -> Action ()
langSinkWriteLn (MkLangSink sink) = sinkWriteLn sink

-- LangSource
newtype LangSource a =
    MkLangSource (Source Action a)
    deriving (Functor)

instance MaybeRepresentational LangSource where
    maybeRepresentational = Nothing

instance HasVariance LangSource where
    type VarianceOf LangSource = 'Covariance

sourceGroundType :: QGroundType '[ CoCCRVariance] LangSource
sourceGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangSource)|]) "Source"

instance HasQGroundType '[ CoCCRVariance] LangSource where
    qGroundType = sourceGroundType

liftSource :: Source IO a -> LangSource a
liftSource source = MkLangSource $ hoistSource liftIO source

langSourceReady :: forall a. LangSource a -> Action Bool
langSourceReady (MkLangSource source) = sourceHasData source

langSourceRead :: forall a. LangSource a -> Action (ItemOrEnd a)
langSourceRead (MkLangSource source) = sourceTake source

langSourceReadAvailable :: forall a. LangSource a -> Action (Maybe (ItemOrEnd a))
langSourceReadAvailable (MkLangSource source) = sourceTakeAvailable source

langSourceReadAllAvailable :: forall a. LangSource a -> Action ([a], Bool)
langSourceReadAllAvailable (MkLangSource source) = sourceTakeAllAvailable source

langSourceGather :: forall a. LangSource a -> Action [a]
langSourceGather (MkLangSource source) = sourceGather source

langConnectSourceSink :: forall a. LangSource a -> LangSink a -> Action ()
langConnectSourceSink (MkLangSource source) (MkLangSink sink) = connectSourceSink source sink

langCreatePipe :: forall a. IO (LangSink a, LangSource a)
langCreatePipe = do
    (sink, source) <- createPipe
    return (liftSink sink, liftSource source)

lineBufferSource :: LangSource Text -> IO (LangSource Text)
lineBufferSource (MkLangSource source) = do
    rs <- filterSource lineBufferFilter source
    return $ MkLangSource rs

langListSource :: forall a. [a] -> IO (LangSource a)
langListSource aa = fmap liftSource $ listSource aa

streamLibraryModule :: LibraryModule context
streamLibraryModule =
    MkLibraryModule $
    MkDocTree
        "Stream"
        "Sinks and sources."
        [ docTreeEntry
              "ItemOrEnd"
              ""
              [ mkTypeEntry "ItemOrEnd" "Either an item, or end (meaning end of stream)." $
                MkSomeGroundType endOrItemGroundType
              , mkValPatEntry "Item" "Construct an `ItemOrEnd` representing an item." (Item @A) $
                ImpureFunction $ \(v :: ItemOrEnd A) ->
                    case v of
                        Item a -> Just (a, ())
                        _ -> Nothing
              , mkValPatEntry "End" "Construct an `ItemOrEnd` representing end of stream." (End @BottomType) $
                ImpureFunction $ \(v :: ItemOrEnd A) ->
                    case v of
                        End -> Just ()
                        _ -> Nothing
              ]
        , docTreeEntry
              "Sink"
              ""
              [ mkTypeEntry "Sink" "A sink is something you can write data (and \"end\") to." $
                MkSomeGroundType sinkGroundType
              , mkValPatEntry "MkSink" "Construct a `Sink` from a function." (toLangSink @A) $
                PureFunction $ \v -> (fromLangSink @A v, ())
              , mkValEntry "mapSink" "" $ contramap @LangSink @A @B
              , mkValEntry "write" "Write an item to a sink." $ langSinkWrite @A
              , mkValEntry "writeEnd" "Write end to a sink. You should not write to the sink after this." $
                langSinkWriteEnd @BottomType
              , mkValEntry "writeLn" "Write text followed by a newline to a text sink." langSinkWriteLn
              ]
        , docTreeEntry
              "Source"
              ""
              [ mkTypeEntry "Source" "A source is something you can read data from." $ MkSomeGroundType sourceGroundType
              , mkValEntry "mapSource" "" $ fmap @LangSource @A @B
              , mkValEntry "isReady" "Does this source have data available now?" $ langSourceReady @TopType
              , mkValEntry "read" "Read data (or end), waiting if necessary." $ langSourceRead @A
              , mkValEntry "readAvailable" "Read data (or end), if it is now available." $ langSourceReadAvailable @A
              , mkValEntry "readAllAvailable" "Read all data now available. Second value is set if end was read." $
                langSourceReadAllAvailable @A
              , mkValEntry "gather" "Gather all data (until end) from a source." $ langSourceGather @A
              , mkValEntry "listSource" "Create a source for a list of items." $ langListSource @A
              , mkValEntry
                    "connect"
                    "Read all data (until end) from a source and write it to a sink, as it becomes available. Does not write end to the sink." $
                langConnectSourceSink @A
              , mkValEntry
                    "createPipe"
                    "Create a pipe. Data written to the sink will be added to a buffer, which can be read from with the source. Can be used to transfer data between asynchronous tasks." $
                langCreatePipe @A
              , mkValEntry
                    "lineBufferSource"
                    "Get a line-buffering source from a text source. Each read will be exactly one line."
                    lineBufferSource
              ]
        ]
