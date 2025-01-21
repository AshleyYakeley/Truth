module Changes.Core.Types.Bi.ChangeMap where

import Shapes

import Changes.Core.Edit
import Changes.Core.Types.List
import Changes.Core.Types.Whole

data ChangeMap updatea updateb = MkChangeMap
    { chmapUpdate :: updatea -> updateb
    , chmapEdit :: UpdateEdit updatea -> UpdateEdit updateb
    , chmapRead ::
        forall tb.
        UpdateReader updateb tb ->
        forall r.
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
    in MkChangeMap{..}

listChangeMap ::
    forall updatea updateb.
    ChangeMap updatea updateb ->
    (UpdateSubject updatea -> UpdateSubject updateb) ->
    ChangeMap (ListUpdate updatea) (ListUpdate updateb)
listChangeMap chmap ab = let
    chmapUpdate' :: ListUpdate updatea -> ListUpdate updateb
    chmapUpdate' (ListUpdateItem i updatea) = ListUpdateItem i $ chmapUpdate chmap updatea
    chmapUpdate' (ListUpdateDelete i) = ListUpdateDelete i
    chmapUpdate' (ListUpdateInsert i a) = ListUpdateInsert i $ chmapSubject chmap a
    chmapUpdate' ListUpdateClear = ListUpdateClear
    chmapEdit' :: ListEdit (UpdateEdit updatea) -> ListEdit (UpdateEdit updateb)
    chmapEdit' (ListEditItem i edita) = ListEditItem i $ chmapEdit chmap edita
    chmapEdit' (ListEditDelete i) = ListEditDelete i
    chmapEdit' (ListEditInsert i a) = ListEditInsert i $ chmapSubject chmap a
    chmapEdit' ListEditClear = ListEditClear
    chmapRead' ::
        forall tb.
        ListReader (UpdateReader updateb) tb ->
        forall r.
        (forall ta. ListReader (UpdateReader updatea) ta -> (ta -> tb) -> r) -> r
    chmapRead' ListReadLength call = call ListReadLength id
    chmapRead' (ListReadItem i rdb) call = chmapRead chmap rdb $ \rda conv -> call (ListReadItem i rda) $ fmap conv
    chmapSubject' :: Vector (UpdateSubject updatea) -> Vector (UpdateSubject updateb)
    chmapSubject' = fmap ab
    in MkChangeMap chmapUpdate' chmapEdit' chmapRead' chmapSubject'

listWholeChangeMap :: forall a b. (a -> b) -> ChangeMap (ListUpdate (WholeUpdate a)) (ListUpdate (WholeUpdate b))
listWholeChangeMap ab = listChangeMap (wholeChangeMap ab) ab
