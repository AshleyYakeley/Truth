module Pinafore.Language.Library.Std.Streams
    ( streamsLibEntries
    ) where

import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Pinafore.Language.Library.Std.Tasks
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

langStdOut :: LangSink Text
langStdOut = MkLangSink $ hoistSink liftIO stdoutTextSink

langStdErr :: LangSink Text
langStdErr = MkLangSink $ hoistSink liftIO stderrTextSink

langGather :: forall a. IO ((LangSink a, IO [a]), LangTask [a])
langGather = do
    (sink, watch, task) <- gatherSink
    return ((liftSink sink, watch), liftTask task)

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

langStdIn :: LangSource Text
langStdIn = MkLangSource $ hoistSource liftIO stdinTextSource

streamsLibEntries :: [DocTreeEntry BindDoc]
streamsLibEntries =
    [ docTreeEntry
          "Sinks & Sources"
          ""
          [ mkTypeEntry "Sink" "A sink is something you can write values (and \"end\") to." $
            MkSomeGroundType sinkGroundType
          , mkValEntry "mapSink" "" $ contramap @LangSink @A @B
          , mkValEntry "sinkWrite" "" $ langSinkWrite @A
          , mkValEntry "sinkWriteEnd" "" $ langSinkWriteEnd @BottomType
          , mkValEntry "sinkWriteLn" "" langSinkWriteLn
          , mkValEntry "gatheringSink" "" $ langGather @A
          , mkValEntry "stdout" "" langStdOut
          , mkValEntry "stderr" "" langStdErr
          , mkTypeEntry "Source" "A source is something you can listen to data from." $
            MkSomeGroundType sourceGroundType
          , mkValEntry "mapSource" "" $ fmap @LangSource @A @B
          , mkValEntry "sourceReady" "" $ langSourceReady @TopType
          , mkValEntry "sourceRead" "" $ langSourceRead @A
          , mkValEntry "sourceReadAvailable" "" $ langSourceReadAvailable @A
          , mkValEntry "sourceReadAllAvailable" "" $ langSourceReadAllAvailable @A
          , mkValEntry "sourceGather" "" $ langSourceGather @A
          , mkValEntry "connect" "" $ langConnectSourceSink @A
          , mkValEntry "createPipe" "" $ langCreatePipe @A
          , mkValEntry "lineBufferSource" "" lineBufferSource
          , mkValEntry "stdin" "" langStdIn
          ]
    ]
