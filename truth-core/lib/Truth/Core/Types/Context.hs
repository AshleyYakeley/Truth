module Truth.Core.Types.Context where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Tuple

--import Truth.Core.Types.Unit
data WithContext context content =
    MkWithContext context
                  content

instance Functor (WithContext context) where
    fmap ab (MkWithContext context a) = MkWithContext context (ab a)

instance Foldable (WithContext context) where
    foldMap am (MkWithContext _ a) = am a

instance Traversable (WithContext context) where
    traverse afb (MkWithContext context a) = fmap (MkWithContext context) (afb a)
    sequenceA (MkWithContext context fa) = fmap (MkWithContext context) fa

instance Comonad (WithContext context) where
    extract (MkWithContext _ content) = content
    extend wab wa@(MkWithContext context _) = MkWithContext context $ wab wa

instance (HasNewValue context, HasNewValue content) => HasNewValue (WithContext context content) where
    newValue = MkWithContext newValue newValue

data WithContextSelector (editx :: *) (editn :: *) (edit :: *) where
    EditContext :: WithContextSelector editx editn editx
    EditContent :: WithContextSelector editx editn editn

instance TestEquality (WithContextSelector ea eb) where
    testEquality EditContext EditContext = Just Refl
    testEquality EditContent EditContent = Just Refl
    testEquality _ _ = Nothing

instance (SubjectReader (EditReader editx), SubjectReader (EditReader editn)) =>
         SubjectTupleSelector (WithContextSelector editx editn) where
    type TupleSubject (WithContextSelector editx editn) = WithContext (EditSubject editx) (EditSubject editn)
    tupleReadFromSubject EditContext (MkWithContext x _n) = x
    tupleReadFromSubject EditContent (MkWithContext _x n) = n

instance FiniteTupleSelector (WithContextSelector ex en) where
    tupleConstruct f = MkWithContext <$> f EditContext <*> f EditContent

instance (c (EditReader ex), c (EditReader en)) => TupleReaderWitness c (WithContextSelector ex en) where
    tupleReaderWitness _ EditContext = Dict
    tupleReaderWitness _ EditContent = Dict

instance (c ex, c en) => TupleWitness c (WithContextSelector ex en) where
    tupleWitness _ EditContext = Dict
    tupleWitness _ EditContent = Dict

type ContextEditReader x n = TupleEditReader (WithContextSelector x n)

type ContextEdit x n = TupleEdit (WithContextSelector x n)

contextEditLens :: EditLens' (ContextEdit editx editn) editx
contextEditLens = tupleEditLens EditContext

contentEditLens :: EditLens' (ContextEdit editx editn) editn
contentEditLens = tupleEditLens EditContent

mapContextEdit :: (edita -> editb) -> ContextEdit editx edita -> ContextEdit editx editb
mapContextEdit _ (MkTupleEdit EditContext edit) = MkTupleEdit EditContext edit
mapContextEdit f (MkTupleEdit EditContent edit) = MkTupleEdit EditContent $ f edit

contextualiseAnEditFunction ::
       forall t edita editb. MonadTransUnlift t
    => AnEditFunction t edita editb
    -> AnEditFunction t edita (ContextEdit edita editb)
contextualiseAnEditFunction (MkAnEditFunction g u) = let
    g' :: ReadFunctionT t (EditReader edita) (ContextEditReader edita editb)
    g' mr (MkTupleEditReader EditContext rt) = lift $ mr rt
    g' mr (MkTupleEditReader EditContent rt) = g mr rt
    u' :: forall m. MonadIO m
       => edita
       -> MutableRead m (EditReader edita)
       -> t m [ContextEdit edita editb]
    u' ea mr =
        withTransConstraintTM @MonadIO $ do
            ebs <- u ea mr
            return $ (MkTupleEdit EditContext ea) : (fmap (MkTupleEdit EditContent) ebs)
    in MkAnEditFunction g' u'

contextualiseEditFunction :: EditFunction' edita editb -> EditFunction' edita (ContextEdit edita editb)
contextualiseEditFunction (MkCloseUnlift unlift f) = MkCloseUnlift unlift $ contextualiseAnEditFunction f

contextualiseAnEditLens ::
       forall t edita editb. MonadTransUnlift t
    => AnEditLens t edita editb
    -> AnEditLens t edita (ContextEdit edita editb)
contextualiseAnEditLens (MkAnEditLens f pe) = let
    f' = contextualiseAnEditFunction f
    pe' :: forall m. MonadIO m
        => ContextEdit edita editb
        -> MutableRead m (EditReader edita)
        -> t m (Maybe [edita])
    pe' (MkTupleEdit EditContext ea) _ = withTransConstraintTM @MonadIO $ return $ Just [ea]
    pe' (MkTupleEdit EditContent eb) mr = pe eb mr
    in MkAnEditLens f' pe'

contextualiseEditLens :: EditLens' edita editb -> EditLens' edita (ContextEdit edita editb)
contextualiseEditLens (MkCloseUnlift unlift lens) = MkCloseUnlift unlift $ contextualiseAnEditLens lens

{-
contextJoinEditFunctions ::
       forall s1 s2 edita editb1 editb2.
       EditFunction s1 edita editb1
    -> EditFunction s2 edita editb2
    -> EditFunction (s1, s2) edita (ContextEdit editb1 editb2)
contextJoinEditFunctions ef1 ef2 =
    MkEditFunction
    { editAccess = pairStateAccess (editAccess ef1) (editAccess ef2)
    , editGet =
          \(cur1, cur2) ->
              \case
                  MkTupleEditReader EditContext rt -> editGet ef1 cur1 rt
                  MkTupleEditReader EditContent rt -> editGet ef2 cur2 rt
    , editUpdate =
          \ea (old1, old2) -> do
              (new1, eb1s) <- editUpdate ef1 ea old1
              (new2, eb2s) <- editUpdate ef2 ea old2
              return ((new1, new2), fmap (MkTupleEdit EditContext) eb1s ++ fmap (MkTupleEdit EditContent) eb2s)
    }

contextJoinEditLenses ::
       forall s1 s2 edita editb1 editb2.
       EditLens s1 edita editb1
    -> EditLens s2 edita editb2
    -> EditLens (s1, s2) edita (ContextEdit editb1 editb2)
contextJoinEditLenses lens1 lens2 =
    MkEditLens
    { editLensFunction = contextJoinEditFunctions (editLensFunction lens1) (editLensFunction lens2)
    , editLensPutEdit =
          \(old1, old2) ->
              \case
                  MkTupleEdit EditContext editb -> do
                      fseas <- editLensPutEdit lens1 old1 editb
                      return $ fmap (\(new1, eas) -> ((new1, old2), eas)) fseas
                  MkTupleEdit EditContent editb -> do
                      fseas <- editLensPutEdit lens2 old2 editb
                      return $ fmap (\(new2, eas) -> ((old1, new2), eas)) fseas
    }

nullContextGeneralLens :: Edit edit => EditLens' edit (ContextEdit UnitEdit edit)
nullContextGeneralLens = MkCloseState $ contextJoinEditLenses unitLens identityState
-}
{-
liftContextReadFunction ::
       forall edita editb editx.
       ReadFunction (EditReader edita) (EditReader editb)
    -> ReadFunction (ContextEditReader editx edita) (ContextEditReader editx editb)
liftContextReadFunction _ (MkTupleEditReader EditContext reader) =
    mapReadable (tupleReadFunction EditContext) $ readable reader
liftContextReadFunction rf (MkTupleEditReader EditContent reader) =
    mapReadable (tupleReadFunction EditContent) $ rf reader

liftContextEditFunction ::
       forall state edita editb editx.
       EditFunction state edita editb
    -> EditFunction state (ContextEdit editx edita) (ContextEdit editx editb)
liftContextEditFunction (MkEditFunction i g u) = let
    g' :: state -> ContextEditReader editx editb t -> Readable (ContextEditReader editx edita) t
    g' cur = liftContextReadFunction $ g cur
    u' :: ContextEdit editx edita
       -> state
       -> Readable (ContextEditReader editx edita) (state, [ContextEdit editx editb])
    u' (MkTupleEdit EditContext edit) old = return (old, [MkTupleEdit EditContext edit])
    u' (MkTupleEdit EditContent edit) old = do
        (new, edits) <- mapReadable (tupleReadFunction EditContent) $ u edit old
        return (new, fmap (MkTupleEdit EditContent) edits)
    in MkEditFunction i g' u'
-}
liftContentAnEditFunction ::
       forall t edita editb editn. MonadTransUnlift t
    => AnEditFunction t edita editb
    -> AnEditFunction t (ContextEdit edita editn) (ContextEdit editb editn)
liftContentAnEditFunction (MkAnEditFunction g u) = let
    g' :: ReadFunctionT t (ContextEditReader edita editn) (ContextEditReader editb editn)
    g' mr (MkTupleEditReader EditContent rt) = lift $ mr $ MkTupleEditReader EditContent rt
    g' mr (MkTupleEditReader EditContext rt) = g (mr . MkTupleEditReader EditContext) rt
    u' :: forall m. MonadIO m
       => ContextEdit edita editn
       -> MutableRead m (EditReader (ContextEdit edita editn))
       -> t m [ContextEdit editb editn]
    u' (MkTupleEdit EditContent edit) _ = withTransConstraintTM @MonadIO $ return [MkTupleEdit EditContent edit]
    u' (MkTupleEdit EditContext edit) mr =
        withTransConstraintTM @MonadIO $ do
            edits <- u edit (mr . MkTupleEditReader EditContext)
            return $ fmap (MkTupleEdit EditContext) edits
    in MkAnEditFunction g' u'

liftContentEditFunction ::
       forall edita editb editn.
       EditFunction' edita editb
    -> EditFunction' (ContextEdit edita editn) (ContextEdit editb editn)
liftContentEditFunction (MkCloseUnlift unlift f) = MkCloseUnlift unlift $ liftContentAnEditFunction f

carryContextEditFunction ::
       EditFunction' (ContextEdit editx edita) editb
    -> EditFunction' (ContextEdit editx edita) (ContextEdit editx editb)
carryContextEditFunction func =
    liftContentEditFunction (editLensFunction $ tupleEditLens EditContext) <.> contextualiseEditFunction func

{-
liftContextEditLens ::
       forall state edita editb editx.
       EditLens state edita editb
    -> EditLens state (ContextEdit editx edita) (ContextEdit editx editb)
liftContextEditLens (MkEditLens (ef :: EditFunction state edita editb) pe) = let
    ef' = liftContextEditFunction ef
    pe' :: state
        -> ContextEdit editx editb
        -> Readable (ContextEditReader editx edita) (Maybe (state, [ContextEdit editx edita]))
    pe' old (MkTupleEdit EditContext edit) = return $ pure (old, [MkTupleEdit EditContext edit])
    pe' old (MkTupleEdit EditContent edit) = do
        mnewedits <- mapReadable (tupleReadFunction EditContent) $ pe old edit
        for mnewedits $ \(new, edits) -> return (new, fmap (MkTupleEdit EditContent) edits)
    in MkEditLens ef' pe'

liftContextGeneralLens ::
       forall edita editb editx.
       EditLens' edita editb
    -> EditLens' (ContextEdit editx edita) (ContextEdit editx editb)
liftContextGeneralLens (MkCloseState lens) = MkCloseState $ liftContextEditLens lens

-}
liftContentEditLens ::
       forall edita editb editn. EditLens' edita editb -> EditLens' (ContextEdit edita editn) (ContextEdit editb editn)
liftContentEditLens (MkCloseUnlift (unlift :: Unlift t) (MkAnEditLens f pe)) = let
    f' = liftContentAnEditFunction f
    pe' :: forall m. MonadIO m
        => ContextEdit editb editn
        -> MutableRead m (EditReader (ContextEdit edita editn))
        -> t m (Maybe [ContextEdit edita editn])
    pe' (MkTupleEdit EditContent edit) _ = withTransConstraintTM @MonadIO $ return $ Just [MkTupleEdit EditContent edit]
    pe' (MkTupleEdit EditContext edit) mr =
        withTransConstraintTM @MonadIO $ do
            mnewedits <- pe edit (mr . MkTupleEditReader EditContext)
            return $ fmap (fmap $ MkTupleEdit EditContext) mnewedits
        --for mnewedits $ \edits -> return (new, fmap (MkTupleEdit EditContext) edits)
    in MkCloseUnlift unlift (MkAnEditLens f' pe')

carryContextEditLens ::
       (Edit editx, Edit edita, Edit editb)
    => EditLens' (ContextEdit editx edita) editb
    -> EditLens' (ContextEdit editx edita) (ContextEdit editx editb)
carryContextEditLens lens = liftContentEditLens (tupleEditLens EditContext) <.> contextualiseEditLens lens
