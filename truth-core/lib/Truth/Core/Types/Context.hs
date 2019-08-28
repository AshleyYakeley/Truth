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

data WithContextSelector (editx :: Type) (editn :: Type) (edit :: Type) where
    SelectContext :: WithContextSelector editx editn editx
    SelectContent :: WithContextSelector editx editn editn

instance (c editx, c editn) => WitnessConstraint c (WithContextSelector editx editn) where
    witnessConstraint SelectContext = Dict
    witnessConstraint SelectContent = Dict

instance Show (WithContextSelector editx editn t) where
    show SelectContext = "context"
    show SelectContent = "content"

instance AllWitnessConstraint Show (WithContextSelector editx editn) where
    allWitnessConstraint = Dict

instance TestEquality (WithContextSelector ea eb) where
    testEquality SelectContext SelectContext = Just Refl
    testEquality SelectContent SelectContent = Just Refl
    testEquality _ _ = Nothing

instance (SubjectReader (EditReader editx), SubjectReader (EditReader editn)) =>
             SubjectTupleSelector (WithContextSelector editx editn) where
    type TupleSubject (WithContextSelector editx editn) = WithContext (EditSubject editx) (EditSubject editn)
    tupleReadFromSubject SelectContext (MkWithContext x _n) = x
    tupleReadFromSubject SelectContent (MkWithContext _x n) = n
    tupleWriteToSubject SelectContext x (MkWithContext _ n) = MkWithContext x n
    tupleWriteToSubject SelectContent n (MkWithContext x _) = MkWithContext x n

instance FiniteTupleSelector (WithContextSelector ex en) where
    tupleConstruct f = MkWithContext <$> f SelectContext <*> f SelectContent

instance (c (EditReader ex), c (EditReader en)) => TupleReaderWitness c (WithContextSelector ex en) where
    tupleReaderWitness SelectContext = Dict
    tupleReaderWitness SelectContent = Dict

instance (c ex, c en) => TupleWitness c (WithContextSelector ex en) where
    tupleWitness SelectContext = Dict
    tupleWitness SelectContent = Dict

instance IsFiniteConsWitness (WithContextSelector ex en) where
    type FiniteConsWitness (WithContextSelector ex en) = '[ ex, en]
    toLTW SelectContext = FirstElementType
    toLTW SelectContent = RestElementType FirstElementType
    fromLTW FirstElementType = SelectContext
    fromLTW (RestElementType FirstElementType) = SelectContent
    fromLTW (RestElementType (RestElementType lt)) = never lt

type ContextEditReader x n = TupleEditReader (WithContextSelector x n)

type ContextEdit x n = TupleEdit (WithContextSelector x n)

contextEditLens :: EditLens (ContextEdit editx editn) editx
contextEditLens = tupleEditLens SelectContext

contentEditLens :: EditLens (ContextEdit editx editn) editn
contentEditLens = tupleEditLens SelectContent

mapContextEdit :: (edita -> editb) -> ContextEdit editx edita -> ContextEdit editx editb
mapContextEdit _ (MkTupleEdit SelectContext edit) = MkTupleEdit SelectContext edit
mapContextEdit f (MkTupleEdit SelectContent edit) = MkTupleEdit SelectContent $ f edit

contextualiseAnUpdateFunction ::
       forall t edita editb. MonadTransUnlift t
    => AnUpdateFunction t edita editb
    -> AnUpdateFunction t edita (ContextEdit edita editb)
contextualiseAnUpdateFunction (MkAnUpdateFunction g u) = let
    g' :: ReadFunctionT t (EditReader edita) (ContextEditReader edita editb)
    g' mr (MkTupleEditReader SelectContext rt) = lift $ mr rt
    g' mr (MkTupleEditReader SelectContent rt) = g mr rt
    u' :: forall m. MonadIO m
       => edita
       -> MutableRead m (EditReader edita)
       -> t m [ContextEdit edita editb]
    u' ea mr =
        withTransConstraintTM @MonadIO $ do
            ebs <- u ea mr
            return $ (MkTupleEdit SelectContext ea) : (fmap (MkTupleEdit SelectContent) ebs)
    in MkAnUpdateFunction g' u'

contextualiseUpdateFunction :: UpdateFunction edita editb -> UpdateFunction edita (ContextEdit edita editb)
contextualiseUpdateFunction (MkCloseUnlift unlift f) = MkCloseUnlift unlift $ contextualiseAnUpdateFunction f

partitionContextEdits :: forall ea eb. [ContextEdit ea eb] -> ([ea], [eb])
partitionContextEdits pes = let
    toEither :: ContextEdit ea eb -> Either ea eb
    toEither (MkTupleEdit SelectContext ea) = Left ea
    toEither (MkTupleEdit SelectContent eb) = Right eb
    in partitionEithers $ fmap toEither pes

contextualiseAnEditLens ::
       forall t edita editb. MonadTransUnlift t
    => AnEditLens t edita editb
    -> AnEditLens t edita (ContextEdit edita editb)
contextualiseAnEditLens (MkAnEditLens f pe) = let
    f' = contextualiseAnUpdateFunction f
    pe' :: forall m. MonadIO m
        => [ContextEdit edita editb]
        -> MutableRead m (EditReader edita)
        -> t m (Maybe [edita])
    pe' edits mr =
        case partitionContextEdits edits of
            (eas, ebs) ->
                withTransConstraintTM @MonadIO $
                getComposeM $ do
                    eas' <- MkComposeM $ pe ebs mr
                    return $ eas ++ eas'
    in MkAnEditLens f' pe'

contextualiseEditLens :: EditLens edita editb -> EditLens edita (ContextEdit edita editb)
contextualiseEditLens (MkCloseUnlift unlift lens) = MkCloseUnlift unlift $ contextualiseAnEditLens lens

liftContentAnUpdateFunction ::
       forall t edita editb editn. MonadTransUnlift t
    => AnUpdateFunction t edita editb
    -> AnUpdateFunction t (ContextEdit edita editn) (ContextEdit editb editn)
liftContentAnUpdateFunction (MkAnUpdateFunction g u) = let
    g' :: ReadFunctionT t (ContextEditReader edita editn) (ContextEditReader editb editn)
    g' mr (MkTupleEditReader SelectContent rt) = lift $ mr $ MkTupleEditReader SelectContent rt
    g' mr (MkTupleEditReader SelectContext rt) = g (mr . MkTupleEditReader SelectContext) rt
    u' :: forall m. MonadIO m
       => ContextEdit edita editn
       -> MutableRead m (EditReader (ContextEdit edita editn))
       -> t m [ContextEdit editb editn]
    u' (MkTupleEdit SelectContent edit) _ = withTransConstraintTM @MonadIO $ return [MkTupleEdit SelectContent edit]
    u' (MkTupleEdit SelectContext edit) mr =
        withTransConstraintTM @MonadIO $ do
            edits <- u edit (mr . MkTupleEditReader SelectContext)
            return $ fmap (MkTupleEdit SelectContext) edits
    in MkAnUpdateFunction g' u'

liftContentUpdateFunction ::
       forall edita editb editn.
       UpdateFunction edita editb
    -> UpdateFunction (ContextEdit edita editn) (ContextEdit editb editn)
liftContentUpdateFunction (MkCloseUnlift unlift f) = MkCloseUnlift unlift $ liftContentAnUpdateFunction f

carryContextUpdateFunction ::
       UpdateFunction (ContextEdit editx edita) editb
    -> UpdateFunction (ContextEdit editx edita) (ContextEdit editx editb)
carryContextUpdateFunction func =
    liftContentUpdateFunction (editLensFunction $ tupleEditLens SelectContext) . contextualiseUpdateFunction func

liftContentAnEditLens ::
       forall t edita editb editn. MonadTransUnlift t
    => AnEditLens t edita editb
    -> AnEditLens t (ContextEdit edita editn) (ContextEdit editb editn)
liftContentAnEditLens (MkAnEditLens f pe) = let
    f' = liftContentAnUpdateFunction f
    pe' :: forall m. MonadIO m
        => [ContextEdit editb editn]
        -> MutableRead m (EditReader (ContextEdit edita editn))
        -> t m (Maybe [ContextEdit edita editn])
    pe' edits mr =
        case partitionContextEdits edits of
            (exs, ens) ->
                withTransConstraintTM @MonadIO $
                getComposeM $ do
                    es1 <- MkComposeM $ pe exs (mr . MkTupleEditReader SelectContext)
                    return $ (fmap (MkTupleEdit SelectContext) es1) ++ (fmap (MkTupleEdit SelectContent) ens)
    in MkAnEditLens f' pe'

liftContentEditLens ::
       forall edita editb editn. EditLens edita editb -> EditLens (ContextEdit edita editn) (ContextEdit editb editn)
liftContentEditLens (MkCloseUnlift unlift alens) = MkCloseUnlift unlift $ liftContentAnEditLens alens

carryContextEditLens ::
       (ApplicableEdit editx, ApplicableEdit edita, ApplicableEdit editb)
    => EditLens (ContextEdit editx edita) editb
    -> EditLens (ContextEdit editx edita) (ContextEdit editx editb)
carryContextEditLens lens = liftContentEditLens (tupleEditLens SelectContext) . contextualiseEditLens lens
