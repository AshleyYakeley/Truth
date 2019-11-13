module Truth.Core.Types.Context where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Resource
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

contextEditLens :: EditLens (ContextUpdate updateX updateN) updateX
contextEditLens = tupleEditLens SelectContext

contentEditLens :: EditLens (ContextUpdate updateX updateN) updateN
contentEditLens = tupleEditLens SelectContent

mapContextEdit ::
       (UpdateEdit updateA -> UpdateEdit updateB)
    -> ContextUpdateEdit updateX updateA
    -> ContextUpdateEdit updateX updateB
mapContextEdit _ (MkTupleUpdateEdit SelectContext edit) = MkTupleUpdateEdit SelectContext edit
mapContextEdit f (MkTupleUpdateEdit SelectContent edit) = MkTupleUpdateEdit SelectContent $ f edit

contextualiseAnUpdateFunction ::
       forall tt updateX updateN. MonadTransStackUnliftAll tt
    => AnUpdateFunction tt updateX updateN
    -> AnUpdateFunction tt updateX (ContextUpdate updateX updateN)
contextualiseAnUpdateFunction (MkAnUpdateFunction g u) = let
    g' :: ReadFunctionTT tt (UpdateReader updateX) (ContextUpdateReader updateX updateN)
    g' mr (MkTupleUpdateReader SelectContext rt) = stackLift @tt $ mr rt
    g' mr (MkTupleUpdateReader SelectContent rt) = g mr rt
    u' :: forall m. MonadIO m
       => updateX
       -> MutableRead m (UpdateReader updateX)
       -> ApplyStack tt m [ContextUpdate updateX updateN]
    u' ea mr =
        case transStackDict @MonadIO @tt @m of
            Dict -> do
                ebs <- u ea mr
                return $ (MkTupleUpdate SelectContext ea) : (fmap (MkTupleUpdate SelectContent) ebs)
    in MkAnUpdateFunction g' u'

contextualiseUpdateFunction :: UpdateFunction edita editb -> UpdateFunction edita (ContextUpdate edita editb)
contextualiseUpdateFunction (MkRunnable2 trun f) =
    case transStackRunnerUnliftAllDict trun of
        Dict -> MkRunnable2 trun $ contextualiseAnUpdateFunction f

partitionContextEdits ::
       forall updateX updateN. [ContextUpdateEdit updateX updateN] -> ([UpdateEdit updateX], [UpdateEdit updateN])
partitionContextEdits pes = let
    toEither :: ContextUpdateEdit updateX updateN -> Either (UpdateEdit updateX) (UpdateEdit updateN)
    toEither (MkTupleUpdateEdit SelectContext ea) = Left ea
    toEither (MkTupleUpdateEdit SelectContent eb) = Right eb
    in partitionEithers $ fmap toEither pes

contextualiseAnEditLens ::
       forall tt updateX updateN. MonadTransStackUnliftAll tt
    => AnEditLens tt updateX updateN
    -> AnEditLens tt updateX (ContextUpdate updateX updateN)
contextualiseAnEditLens (MkAnEditLens f pe) = let
    f' = contextualiseAnUpdateFunction f
    pe' :: forall m. MonadIO m
        => [ContextUpdateEdit updateX updateN]
        -> MutableRead m (UpdateReader updateX)
        -> ApplyStack tt m (Maybe [UpdateEdit updateX])
    pe' edits mr =
        case transStackDict @MonadIO @tt @m of
            Dict ->
                case partitionContextEdits edits of
                    (eas, ebs) ->
                        getComposeM $ do
                            eas' <- MkComposeM $ pe ebs mr
                            return $ eas ++ eas'
    in MkAnEditLens f' pe'

contextualiseEditLens :: EditLens updateX updateN -> EditLens updateX (ContextUpdate updateX updateN)
contextualiseEditLens (MkRunnable2 trun lens) =
    case transStackRunnerUnliftAllDict trun of
        Dict -> MkRunnable2 trun $ contextualiseAnEditLens lens

liftContentAnUpdateFunction ::
       forall tt updateA updateB updateN. MonadTransStackUnliftAll tt
    => AnUpdateFunction tt updateA updateB
    -> AnUpdateFunction tt (ContextUpdate updateA updateN) (ContextUpdate updateB updateN)
liftContentAnUpdateFunction (MkAnUpdateFunction g u) = let
    g' :: ReadFunctionTT tt (ContextUpdateReader updateA updateN) (ContextUpdateReader updateB updateN)
    g' mr (MkTupleUpdateReader SelectContent rt) = stackLift @tt $ mr $ MkTupleUpdateReader SelectContent rt
    g' mr (MkTupleUpdateReader SelectContext rt) = g (mr . MkTupleUpdateReader SelectContext) rt
    u' :: forall m. MonadIO m
       => ContextUpdate updateA updateN
       -> MutableRead m (ContextUpdateReader updateA updateN)
       -> ApplyStack tt m [ContextUpdate updateB updateN]
    u' (MkTupleUpdate SelectContent update) _ =
        case transStackDict @MonadIO @tt @m of
            Dict -> return [MkTupleUpdate SelectContent update]
    u' (MkTupleUpdate SelectContext update) mr =
        case transStackDict @MonadIO @tt @m of
            Dict -> do
                updates <- u update (mr . MkTupleUpdateReader SelectContext)
                return $ fmap (MkTupleUpdate SelectContext) updates
    in MkAnUpdateFunction g' u'

liftContentUpdateFunction ::
       forall updateA updateB updateN.
       UpdateFunction updateA updateB
    -> UpdateFunction (ContextUpdate updateA updateN) (ContextUpdate updateB updateN)
liftContentUpdateFunction (MkRunnable2 trun f) =
    case transStackRunnerUnliftAllDict trun of
        Dict -> MkRunnable2 trun $ liftContentAnUpdateFunction f

carryContextUpdateFunction ::
       UpdateFunction (ContextUpdate updateX updateA) updateB
    -> UpdateFunction (ContextUpdate updateX updateA) (ContextUpdate updateX updateB)
carryContextUpdateFunction func =
    liftContentUpdateFunction (editLensFunction $ tupleEditLens SelectContext) . contextualiseUpdateFunction func

liftContentAnEditLens ::
       forall tt updateA updateB updateN. MonadTransStackUnliftAll tt
    => AnEditLens tt updateA updateB
    -> AnEditLens tt (ContextUpdate updateA updateN) (ContextUpdate updateB updateN)
liftContentAnEditLens (MkAnEditLens f pe) = let
    f' = liftContentAnUpdateFunction f
    pe' :: forall m. MonadIO m
        => [ContextUpdateEdit updateB updateN]
        -> MutableRead m (ContextUpdateReader updateA updateN)
        -> ApplyStack tt m (Maybe [ContextUpdateEdit updateA updateN])
    pe' edits mr =
        case transStackDict @MonadIO @tt @m of
            Dict ->
                case partitionContextEdits edits of
                    (exs, ens) ->
                        getComposeM $ do
                            es1 <- MkComposeM $ pe exs (mr . MkTupleUpdateReader SelectContext)
                            return $
                                (fmap (MkTupleUpdateEdit SelectContext) es1) ++
                                (fmap (MkTupleUpdateEdit SelectContent) ens)
    in MkAnEditLens f' pe'

liftContentEditLens ::
       forall updateA updateB updateN.
       EditLens updateA updateB
    -> EditLens (ContextUpdate updateA updateN) (ContextUpdate updateB updateN)
liftContentEditLens (MkRunnable2 trun alens) =
    case transStackRunnerUnliftAllDict trun of
        Dict -> MkRunnable2 trun $ liftContentAnEditLens alens

carryContextEditLens ::
       EditLens (ContextUpdate updateX updateA) updateB
    -> EditLens (ContextUpdate updateX updateA) (ContextUpdate updateX updateB)
carryContextEditLens lens = liftContentEditLens (tupleEditLens SelectContext) . contextualiseEditLens lens
