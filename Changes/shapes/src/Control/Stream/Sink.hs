module Control.Stream.Sink where

import Control.Stream.Filter
import Control.Task
import Shapes.Import

newtype Sink m a =
    MkSink (EndOrItem a -> m ())

instance Contravariant (Sink m) where
    contramap ab (MkSink f) = MkSink $ \a -> f $ fmap ab a

instance Monad m => Semigroup (Sink m a) where
    MkSink f1 <> MkSink f2 = MkSink $ \ea -> f1 ea >> f2 ea

instance Monad m => Monoid (Sink m a) where
    mempty = MkSink $ \_ -> return ()

hoistSink :: (m1 --> m2) -> Sink m1 --> Sink m2
hoistSink mm (MkSink f) = MkSink $ \ma -> mm $ f ma

sinkWrite :: Sink m a -> a -> m ()
sinkWrite (MkSink f) a = f $ Item a

sinkWriteEnd :: Sink m a -> m ()
sinkWriteEnd (MkSink f) = f End

sinkWriteLn :: Sink m Text -> Text -> m ()
sinkWriteLn sink t = sinkWrite sink $ t <> "\n"

filterSink ::
       forall m a b. MonadIO m
    => Filter m a b
    -> Sink m b
    -> IO (Sink m a)
filterSink (MkFilter s0 f) sink = do
    var <- newMVar s0
    return $
        MkSink $ \ma -> do
            bb <- dangerousMVarRunStateT var $ f ma
            for_ bb $ sinkWrite sink
            case ma of
                End -> sinkWriteEnd sink
                Item _ -> return ()

gatherSink :: forall a. IO (Sink IO a, IO [a], Task IO [a])
gatherSink = do
    resultVar <- newEmptyMVar
    bufferVar <- newMVar []
    let
        sink =
            MkSink $ \case
                End -> do
                    aa <- takeMVar bufferVar
                    putMVar resultVar aa
                Item a -> do
                    aa <- takeMVar bufferVar
                    putMVar bufferVar $ aa <> [a]
    return (sink, readMVar bufferVar, mvarTask resultVar)

handleSink :: Handle -> Sink IO StrictByteString
handleSink h =
    MkSink $ \case
        End -> hClose h
        Item t -> hPut h $ fromStrict t

handleSinkText :: Handle -> Sink IO Text
handleSinkText h = contramap encodeUtf8 $ handleSink h

stdoutTextSink :: Sink IO Text
stdoutTextSink = handleSinkText stdout

stderrTextSink :: Sink IO Text
stderrTextSink = handleSinkText stderr
