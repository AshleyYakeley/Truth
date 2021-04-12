module Changes.Core.Types.Bi.ChangeMap where

import Changes.Core.Edit
import Changes.Core.Sequence
import Changes.Core.Types.List
import Changes.Core.Types.Whole
import Shapes

data ChangeMap updatea updateb = MkChangeMap
    { chmapUpdate :: updatea -> updateb
    , chmapEdit :: UpdateEdit updatea -> UpdateEdit updateb
    , chmapRead :: forall tb.
                           UpdateReader updateb tb -> forall r.
                                                              (forall ta. UpdateReader updatea ta -> (ta -> tb) -> r) -> r
    , chmapSubject :: UpdateSubject updatea -> UpdateSubject updateb
    }

wholeChangeMap :: forall a b. (a -> b) -> ChangeMap (WholeUpdate a) (WholeUpdate b)
wholeChangeMap ab = let
    chmapUpdate :: WholeUpdate a -> WholeUpdate b
    chmapUpdate (MkWholeUpdate a) = MkWholeUpdate $ ab a
    chmapEdit :: WholeEdit a -> WholeEdit b
    chmapEdit (MkWholeReaderEdit a) = MkWholeReaderEdit $ ab a
    chmapRead :: forall tb. WholeReader b tb -> forall r. (forall ta. WholeReader a ta -> (ta -> tb) -> r) -> r
    chmapRead ReadWhole call = call ReadWhole ab
    chmapSubject :: a -> b
    chmapSubject = ab
    in MkChangeMap {..}

listChangeMap ::
       forall updatea updateb a b.
       ChangeMap updatea updateb
    -> (a -> b)
    -> ChangeMap (ListUpdate [a] updatea) (ListUpdate [b] updateb)
listChangeMap chmap ab = let
    chmapUpdate' :: ListUpdate [a] updatea -> ListUpdate [b] updateb
    chmapUpdate' (ListUpdateItem i updatea) = ListUpdateItem (seqPointConvert i) $ chmapUpdate chmap updatea
    chmapUpdate' (ListUpdateDelete i) = ListUpdateDelete (seqPointConvert i)
    chmapUpdate' (ListUpdateInsert i a) = ListUpdateInsert (seqPointConvert i) $ chmapSubject chmap a
    chmapUpdate' ListUpdateClear = ListUpdateClear
    chmapEdit' :: ListEdit [a] (UpdateEdit updatea) -> ListEdit [b] (UpdateEdit updateb)
    chmapEdit' (ListEditItem i edita) = ListEditItem (seqPointConvert i) $ chmapEdit chmap edita
    chmapEdit' (ListEditDelete i) = ListEditDelete (seqPointConvert i)
    chmapEdit' (ListEditInsert i a) = ListEditInsert (seqPointConvert i) $ chmapSubject chmap a
    chmapEdit' ListEditClear = ListEditClear
    chmapRead' ::
           forall tb.
           ListReader [b] (UpdateReader updateb) tb
        -> forall r. (forall ta. ListReader [a] (UpdateReader updatea) ta -> (ta -> tb) -> r) -> r
    chmapRead' ListReadLength call = call ListReadLength seqPointConvert
    chmapRead' (ListReadItem i rdb) call =
        chmapRead chmap rdb $ \rda conv -> call (ListReadItem (seqPointConvert i) rda) $ fmap conv
    chmapSubject' :: [a] -> [b]
    chmapSubject' = fmap ab
    in MkChangeMap chmapUpdate' chmapEdit' chmapRead' chmapSubject'

listWholeChangeMap ::
       forall a b. (a -> b) -> ChangeMap (ListUpdate [a] (WholeUpdate a)) (ListUpdate [b] (WholeUpdate b))
listWholeChangeMap ab = listChangeMap (wholeChangeMap ab) ab
