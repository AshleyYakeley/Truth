module Truth.Core.Read.ReadFunction where

import Truth.Core.Import
import Truth.Core.Read.FullSubjectReader
import Truth.Core.Read.Readable
import Truth.Core.Read.SubjectReader
import Truth.Core.Read.WriterReadable

type ReadFunction readera readerb = forall t. readerb t -> Readable readera t

mapMutableRead ::
       forall m ra rb. (MonadIO m)
    => ReadFunction ra rb
    -> MutableRead m ra
    -> MutableRead m rb
mapMutableRead rfab sma rbt = unReadable (rfab rbt) sma

unmapMutableRead ::
       forall ra rb.
       (forall m. MonadIO m =>
                      MutableRead m ra -> MutableRead m rb)
    -> ReadFunction ra rb
unmapMutableRead mrmr rbt = MkReadable $ \mra -> mrmr mra rbt

mapMutableReadW ::
       forall m ra rb. (MonadIO m)
    => ReadFunction ra rb
    -> MutableReadW m ra
    -> MutableReadW m rb
mapMutableReadW rf (MkMutableReadW mr) = MkMutableReadW $ mapMutableRead rf mr

composeReadFunction :: forall ra rb rc. ReadFunction rb rc -> ReadFunction ra rb -> ReadFunction ra rc
composeReadFunction = mapMutableRead

makeReadFunction :: (SubjectReader rb) => Readable ra (ReaderSubject rb) -> ReadFunction ra rb
makeReadFunction = readFromSubjectM

simpleReadFunction ::
       (FullSubjectReader ra, SubjectReader rb) => (ReaderSubject ra -> ReaderSubject rb) -> ReadFunction ra rb
simpleReadFunction ab = makeReadFunction (fmap ab subjectFromReader)

convertReadFunction ::
       (FullSubjectReader ra, SubjectReader rb, ReaderSubject ra ~ ReaderSubject rb) => ReadFunction ra rb
convertReadFunction = simpleReadFunction id

mapGenReadable :: forall ra rb t. ReadFunction ra rb -> Readable rb t -> Readable ra t
mapGenReadable rf (MkReadable srbmt) = srbmt $ \rt -> rf rt

fromReadFunctionM ::
       forall m ra rb. (MonadIO m, SubjectReader ra, FullSubjectReader rb)
    => ReadFunction ra rb
    -> m (ReaderSubject ra)
    -> m (ReaderSubject rb)
fromReadFunctionM rf mra = unReadable (mapGenReadable rf subjectFromReader) $ readFromSubjectM mra

mapStructureF :: MonadIO m => ReadFunctionF f ra rb -> MutableRead m ra -> MutableRead (Compose m f) rb
mapStructureF rff sa rbt = Compose $ unReadable (rff rbt) sa

type ReadFunctionF f readera readerb = forall t. readerb t -> Readable readera (f t)

class MapReadable readable where
    mapReadable :: ReadFunction ra rb -> readable rb t -> readable ra t
    mapReadableF :: (Monad f, Traversable f) => ReadFunctionF f ra rb -> readable rb t -> readable ra (f t)

instance MapReadable Readable where
    mapReadable (rf :: ReadFunction ra rb) (MkReadable srbmt) = srbmt $ \rt -> rf rt
    mapReadableF (rff :: ReadFunctionF f ra rb) (MkReadable srbmt) = getCompose $ srbmt $ \rt -> Compose $ rff rt

instance MapReadable (WriterReadable w) where
    mapReadable rf (MkWriterReadable sbwt) = MkWriterReadable $ \sa wm -> sbwt (mapMutableRead rf sa) wm
    mapReadableF (rff :: ReadFunctionF f ra rb) (MkWriterReadable sbwt) =
        MkWriterReadable $ \(sa :: MutableRead m ra) wm ->
            getCompose $ sbwt (mapStructureF rff sa) (fmap (Compose . fmap pure) wm)

writerToReadable :: forall w reader. WriterReadable w reader () -> Readable reader [w]
writerToReadable (MkWriterReadable swma) =
    MkReadable $ \(s :: forall t. reader t -> m t) -> execWriterT $ swma (fmap lift s) (tell . pure)
