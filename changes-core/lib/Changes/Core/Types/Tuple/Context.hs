module Truth.Core.Types.Tuple.Context where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Types.Tuple.Tuple

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

data WithContextSelector (updateX :: Type) (updateN :: Type) (update :: Type) where
    SelectContext :: WithContextSelector updateX updateN updateX
    SelectContent :: WithContextSelector updateX updateN updateN

instance (c updateX, c updateN) => WitnessConstraint c (WithContextSelector updateX updateN) where
    witnessConstraint SelectContext = Dict
    witnessConstraint SelectContent = Dict

instance Show (WithContextSelector updateX updateN t) where
    show SelectContext = "context"
    show SelectContent = "content"

instance AllWitnessConstraint Show (WithContextSelector updateX updateN) where
    allWitnessConstraint = Dict

instance TestEquality (WithContextSelector updateX updateN) where
    testEquality SelectContext SelectContext = Just Refl
    testEquality SelectContent SelectContent = Just Refl
    testEquality _ _ = Nothing

instance (SubjectReader (UpdateReader updateX), SubjectReader (UpdateReader updateN)) =>
             SubjectTupleSelector (WithContextSelector updateX updateN) where
    type TupleSubject (WithContextSelector updateX updateN) = WithContext (UpdateSubject updateX) (UpdateSubject updateN)
    tupleReadFromSubject SelectContext (MkWithContext x _n) = x
    tupleReadFromSubject SelectContent (MkWithContext _x n) = n
    tupleWriteToSubject SelectContext x (MkWithContext _ n) = MkWithContext x n
    tupleWriteToSubject SelectContent n (MkWithContext x _) = MkWithContext x n

instance FiniteTupleSelector (WithContextSelector updateX updateN) where
    tupleConstruct f = MkWithContext <$> f SelectContext <*> f SelectContent

instance (c (UpdateReader updateX), c (UpdateReader updateN)) =>
             TupleReaderWitness c (WithContextSelector updateX updateN) where
    tupleReaderWitness SelectContext = Dict
    tupleReaderWitness SelectContent = Dict

instance (c (UpdateEdit updateX), c (UpdateEdit updateN)) => TupleEditWitness c (WithContextSelector updateX updateN) where
    tupleEditWitness SelectContext = Dict
    tupleEditWitness SelectContent = Dict

instance (c updateX, c updateN) => TupleUpdateWitness c (WithContextSelector updateX updateN) where
    tupleUpdateWitness SelectContext = Dict
    tupleUpdateWitness SelectContent = Dict

instance IsFiniteConsWitness (WithContextSelector updateX updateN) where
    type FiniteConsWitness (WithContextSelector updateX updateN) = '[ updateX, updateN]
    toLTW SelectContext = FirstElementType
    toLTW SelectContent = RestElementType FirstElementType
    fromLTW FirstElementType = SelectContext
    fromLTW (RestElementType FirstElementType) = SelectContent
    fromLTW (RestElementType (RestElementType lt)) = never lt

type ContextUpdateReader updateX updateN = TupleUpdateReader (WithContextSelector updateX updateN)

type ContextUpdateEdit updateX updateN = TupleUpdateEdit (WithContextSelector updateX updateN)

type ContextUpdate updateX updateN = TupleUpdate (WithContextSelector updateX updateN)

contextChangeLens :: ChangeLens (ContextUpdate updateX updateN) updateX
contextChangeLens = tupleChangeLens SelectContext

contentChangeLens :: ChangeLens (ContextUpdate updateX updateN) updateN
contentChangeLens = tupleChangeLens SelectContent

mapContextEdit ::
       (UpdateEdit updateA -> UpdateEdit updateB)
    -> ContextUpdateEdit updateX updateA
    -> ContextUpdateEdit updateX updateB
mapContextEdit _ (MkTupleUpdateEdit SelectContext edit) = MkTupleUpdateEdit SelectContext edit
mapContextEdit f (MkTupleUpdateEdit SelectContent edit) = MkTupleUpdateEdit SelectContent $ f edit

partitionContextEdits ::
       forall updateX updateN. [ContextUpdateEdit updateX updateN] -> ([UpdateEdit updateX], [UpdateEdit updateN])
partitionContextEdits pes = let
    toEither :: ContextUpdateEdit updateX updateN -> Either (UpdateEdit updateX) (UpdateEdit updateN)
    toEither (MkTupleUpdateEdit SelectContext ea) = Left ea
    toEither (MkTupleUpdateEdit SelectContent eb) = Right eb
    in partitionEithers $ fmap toEither pes

contextualiseChangeLens ::
       forall updateX updateN. ChangeLens updateX updateN -> ChangeLens updateX (ContextUpdate updateX updateN)
contextualiseChangeLens (MkChangeLens g u pe) = let
    g' :: ReadFunction (UpdateReader updateX) (ContextUpdateReader updateX updateN)
    g' mr (MkTupleUpdateReader SelectContext rt) = mr rt
    g' mr (MkTupleUpdateReader SelectContent rt) = g mr rt
    u' :: forall m. MonadIO m
       => updateX
       -> Readable m (UpdateReader updateX)
       -> m [ContextUpdate updateX updateN]
    u' ea mr = do
        ebs <- u ea mr
        return $ (MkTupleUpdate SelectContext ea) : (fmap (MkTupleUpdate SelectContent) ebs)
    pe' :: forall m. MonadIO m
        => [ContextUpdateEdit updateX updateN]
        -> Readable m (UpdateReader updateX)
        -> m (Maybe [UpdateEdit updateX])
    pe' edits mr =
        case partitionContextEdits edits of
            (eas, ebs) ->
                getComposeM $ do
                    eas' <- MkComposeM $ pe ebs mr
                    return $ eas ++ eas'
    in MkChangeLens g' u' pe'

liftContextChangeLens ::
       forall updateX updateA updateB.
       ChangeLens updateA updateB
    -> ChangeLens (ContextUpdate updateX updateA) (ContextUpdate updateX updateB)
liftContextChangeLens (MkChangeLens g u pe) = let
    g' :: ReadFunction (ContextUpdateReader updateX updateA) (ContextUpdateReader updateX updateB)
    g' mr (MkTupleUpdateReader SelectContext rt) = mr $ MkTupleUpdateReader SelectContext rt
    g' mr (MkTupleUpdateReader SelectContent rt) = g (mr . MkTupleUpdateReader SelectContent) rt
    u' :: forall m. MonadIO m
       => ContextUpdate updateX updateA
       -> Readable m (ContextUpdateReader updateX updateA)
       -> m [ContextUpdate updateX updateB]
    u' (MkTupleUpdate SelectContext update) _ = return [MkTupleUpdate SelectContext update]
    u' (MkTupleUpdate SelectContent update) mr = do
        updates <- u update (mr . MkTupleUpdateReader SelectContent)
        return $ fmap (MkTupleUpdate SelectContent) updates
    pe' :: forall m. MonadIO m
        => [ContextUpdateEdit updateX updateB]
        -> Readable m (ContextUpdateReader updateX updateA)
        -> m (Maybe [ContextUpdateEdit updateX updateA])
    pe' edits mr =
        case partitionContextEdits edits of
            (exs, ens) ->
                getComposeM $ do
                    es1 <- MkComposeM $ pe ens (mr . MkTupleUpdateReader SelectContent)
                    return $
                        (fmap (MkTupleUpdateEdit SelectContent) es1) ++ (fmap (MkTupleUpdateEdit SelectContext) exs)
    in MkChangeLens g' u' pe'

liftContentChangeLens ::
       forall updateA updateB updateN.
       ChangeLens updateA updateB
    -> ChangeLens (ContextUpdate updateA updateN) (ContextUpdate updateB updateN)
liftContentChangeLens (MkChangeLens g u pe) = let
    g' :: ReadFunction (ContextUpdateReader updateA updateN) (ContextUpdateReader updateB updateN)
    g' mr (MkTupleUpdateReader SelectContent rt) = mr $ MkTupleUpdateReader SelectContent rt
    g' mr (MkTupleUpdateReader SelectContext rt) = g (mr . MkTupleUpdateReader SelectContext) rt
    u' :: forall m. MonadIO m
       => ContextUpdate updateA updateN
       -> Readable m (ContextUpdateReader updateA updateN)
       -> m [ContextUpdate updateB updateN]
    u' (MkTupleUpdate SelectContent update) _ = return [MkTupleUpdate SelectContent update]
    u' (MkTupleUpdate SelectContext update) mr = do
        updates <- u update (mr . MkTupleUpdateReader SelectContext)
        return $ fmap (MkTupleUpdate SelectContext) updates
    pe' :: forall m. MonadIO m
        => [ContextUpdateEdit updateB updateN]
        -> Readable m (ContextUpdateReader updateA updateN)
        -> m (Maybe [ContextUpdateEdit updateA updateN])
    pe' edits mr =
        case partitionContextEdits edits of
            (exs, ens) ->
                getComposeM $ do
                    es1 <- MkComposeM $ pe exs (mr . MkTupleUpdateReader SelectContext)
                    return $
                        (fmap (MkTupleUpdateEdit SelectContext) es1) ++ (fmap (MkTupleUpdateEdit SelectContent) ens)
    in MkChangeLens g' u' pe'

carryContextChangeLens ::
       ChangeLens (ContextUpdate updateX updateA) updateB
    -> ChangeLens (ContextUpdate updateX updateA) (ContextUpdate updateX updateB)
carryContextChangeLens lens = liftContentChangeLens (tupleChangeLens SelectContext) . contextualiseChangeLens lens
