module Truth.Core.Types.Context where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Tuple

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

instance (c editx, c editn) => WitnessConstraint c (WithContextSelector editx editn) where
    witnessConstraint EditContext = Dict
    witnessConstraint EditContent = Dict

instance Show (WithContextSelector editx editn t) where
    show EditContext = "context"
    show EditContent = "content"

instance AllWitnessConstraint Show (WithContextSelector editx editn) where
    allWitnessConstraint = Dict

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

contextEditLens :: EditLens (ContextEdit editx editn) editx
contextEditLens = tupleEditLens EditContext

contentEditLens :: EditLens (ContextEdit editx editn) editn
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

contextualiseEditFunction :: EditFunction edita editb -> EditFunction edita (ContextEdit edita editb)
contextualiseEditFunction (MkCloseUnlift unlift f) = MkCloseUnlift unlift $ contextualiseAnEditFunction f

partitionContextEdits :: forall ea eb. [ContextEdit ea eb] -> ([ea], [eb])
partitionContextEdits pes = let
    toEither :: ContextEdit ea eb -> Either ea eb
    toEither (MkTupleEdit EditContext ea) = Left ea
    toEither (MkTupleEdit EditContent eb) = Right eb
    in partitionEithers $ fmap toEither pes

contextualiseAnEditLens ::
       forall t edita editb. MonadTransUnlift t
    => AnEditLens t edita editb
    -> AnEditLens t edita (ContextEdit edita editb)
contextualiseAnEditLens (MkAnEditLens f pe) = let
    f' = contextualiseAnEditFunction f
    pe' :: forall m. MonadIO m
        => [ContextEdit edita editb]
        -> MutableRead m (EditReader edita)
        -> t m (Maybe [edita])
    pe' edits mr =
        case partitionContextEdits edits of
            (eas, ebs) ->
                withTransConstraintTM @MonadIO $
                getCompose $ do
                    eas' <- Compose $ pe ebs mr
                    return $ eas ++ eas'
    in MkAnEditLens f' pe'

contextualiseEditLens :: EditLens edita editb -> EditLens edita (ContextEdit edita editb)
contextualiseEditLens (MkCloseUnlift unlift lens) = MkCloseUnlift unlift $ contextualiseAnEditLens lens

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
       EditFunction edita editb
    -> EditFunction (ContextEdit edita editn) (ContextEdit editb editn)
liftContentEditFunction (MkCloseUnlift unlift f) = MkCloseUnlift unlift $ liftContentAnEditFunction f

carryContextEditFunction ::
       EditFunction (ContextEdit editx edita) editb -> EditFunction (ContextEdit editx edita) (ContextEdit editx editb)
carryContextEditFunction func =
    liftContentEditFunction (editLensFunction $ tupleEditLens EditContext) . contextualiseEditFunction func

liftContentEditLens ::
       forall edita editb editn. EditLens edita editb -> EditLens (ContextEdit edita editn) (ContextEdit editb editn)
liftContentEditLens (MkCloseUnlift (unlift :: Unlift t) (MkAnEditLens f pe)) = let
    f' = liftContentAnEditFunction f
    pe' :: forall m. MonadIO m
        => [ContextEdit editb editn]
        -> MutableRead m (EditReader (ContextEdit edita editn))
        -> t m (Maybe [ContextEdit edita editn])
    pe' edits mr =
        case partitionContextEdits edits of
            (exs, ens) ->
                withTransConstraintTM @MonadIO $
                getCompose $ do
                    es1 <- Compose $ pe exs (mr . MkTupleEditReader EditContext)
                    return $ (fmap (MkTupleEdit EditContext) es1) ++ (fmap (MkTupleEdit EditContent) ens)
    in MkCloseUnlift unlift (MkAnEditLens f' pe')

carryContextEditLens ::
       (ApplicableEdit editx, ApplicableEdit edita, ApplicableEdit editb)
    => EditLens (ContextEdit editx edita) editb
    -> EditLens (ContextEdit editx edita) (ContextEdit editx editb)
carryContextEditLens lens = liftContentEditLens (tupleEditLens EditContext) . contextualiseEditLens lens
