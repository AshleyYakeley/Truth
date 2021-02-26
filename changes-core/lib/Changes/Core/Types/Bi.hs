module Changes.Core.Types.Bi
    ( module I
    , mappingBiChangeLens
    ) where

import Changes.Core.Edit
import Changes.Core.Lens
import Changes.Core.Read
import Shapes

import Changes.Core.Types.Bi.Bi as I
import Changes.Core.Types.Bi.ChangeMap as I
import Changes.Core.Types.Bi.Whole as I

mappingBiChangeLens ::
       forall updateAP updateBP updateAQ updateBQ.
       ChangeMap updateBP updateAP
    -> ChangeMap updateAQ updateBQ
    -> ChangeLens (BiUpdate updateAP updateAQ) (BiUpdate updateBP updateBQ)
mappingBiChangeLens pmap qmap = let
    clRead :: ReadFunction (UpdateReader updateAQ) (UpdateReader updateBQ)
    clRead rd rt2 = chmapRead qmap rt2 $ \rt1 qq -> fmap qq (rd rt1)
    clUpdate ::
           forall m. MonadIO m
        => BiUpdate updateAP updateAQ
        -> Readable m (UpdateReader updateAQ)
        -> m [BiUpdate updateBP updateBQ]
    clUpdate (MkBiUpdate uq1) _ = return $ pure $ MkBiUpdate $ chmapUpdate qmap uq1
    mapEdit :: BiEdit (UpdateEdit updateBP) (UpdateEdit updateBQ) -> BiEdit (UpdateEdit updateAP) (UpdateEdit updateAQ)
    mapEdit (MkBiEdit edit) = MkBiEdit $ chmapEdit pmap edit
    clPutEdits ::
           forall m. MonadIO m
        => [BiEdit (UpdateEdit updateBP) (UpdateEdit updateBQ)]
        -> Readable m (UpdateReader updateAQ)
        -> m (Maybe [BiEdit (UpdateEdit updateAP) (UpdateEdit updateAQ)])
    clPutEdits edits _ = return $ Just $ fmap mapEdit edits
    in MkChangeLens {..}
