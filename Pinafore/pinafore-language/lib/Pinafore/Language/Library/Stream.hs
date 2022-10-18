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

-- LangSink
newtype LangSink a =
    MkLangSink (Sink PinaforeAction a)
    deriving (Contravariant)

instance MaybeRepresentational LangSink where
    maybeRepresentational = Nothing

instance HasVariance LangSink where
    type VarianceOf LangSink = 'Contravariance

sinkGroundType :: PinaforeGroundType '[ ContraCCRVariance] LangSink
sinkGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangSink)|]) "Sink"

instance HasPinaforeGroundType '[ ContraCCRVariance] LangSink where
    pinaforeGroundType = sinkGroundType

liftSink :: Sink IO a -> LangSink a
liftSink sink = MkLangSink $ hoistSink liftIO sink

langSinkWrite :: forall a. LangSink a -> a -> PinaforeAction ()
langSinkWrite (MkLangSink sink) = sinkWrite sink

langSinkWriteEnd :: forall a. LangSink a -> PinaforeAction ()
langSinkWriteEnd (MkLangSink sink) = sinkWriteEnd sink

langSinkWriteLn :: LangSink Text -> Text -> PinaforeAction ()
langSinkWriteLn (MkLangSink sink) = sinkWriteLn sink

-- LangSource
newtype LangSource a =
    MkLangSource (Source PinaforeAction a)
    deriving (Functor)

instance MaybeRepresentational LangSource where
    maybeRepresentational = Nothing

instance HasVariance LangSource where
    type VarianceOf LangSource = 'Covariance

sourceGroundType :: PinaforeGroundType '[ CoCCRVariance] LangSource
sourceGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangSource)|]) "Source"

instance HasPinaforeGroundType '[ CoCCRVariance] LangSource where
    pinaforeGroundType = sourceGroundType

liftSource :: Source IO a -> LangSource a
liftSource source = MkLangSource $ hoistSource liftIO source

langSourceReady :: forall a. LangSource a -> PinaforeAction Bool
langSourceReady (MkLangSource source) = sourceHasData source

langSourceRead :: forall a. LangSource a -> PinaforeAction (EndOrItem a)
langSourceRead (MkLangSource source) = sourceTake source

langSourceReadAvailable :: forall a. LangSource a -> PinaforeAction (Maybe (EndOrItem a))
langSourceReadAvailable (MkLangSource source) = sourceTakeAvailable source

langSourceReadAllAvailable :: forall a. LangSource a -> PinaforeAction ([a], Bool)
langSourceReadAllAvailable (MkLangSource source) = sourceTakeAllAvailable source

langSourceGather :: forall a. LangSource a -> PinaforeAction [a]
langSourceGather (MkLangSource source) = sourceGather source

langConnectSourceSink :: forall a. LangSource a -> LangSink a -> PinaforeAction ()
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

streamLibraryModule :: LibraryModule
streamLibraryModule =
    MkDocTree
        "Stream"
        "Sinks and sources."
        [ mkTypeEntry "Sink" "A sink is something you can write data (and \"end\") to." $
          MkSomeGroundType sinkGroundType
        , mkValEntry "mapSink" "" $ contramap @LangSink @A @B
        , mkValEntry "write" "Write an item to a sink." $ langSinkWrite @A
        , mkValEntry "writeEnd" "Write end to a sink. You should not write to the sink after this." $
          langSinkWriteEnd @BottomType
        , mkValEntry "writeLn" "Write text followed by a newline to a text sink." langSinkWriteLn
        , mkTypeEntry "Source" "A source is something you can read data from." $ MkSomeGroundType sourceGroundType
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
