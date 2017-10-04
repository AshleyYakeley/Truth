module Truth.Core.Types.Context where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Tuple;
    import Truth.Core.Types.Unit;


    data WithContext context content = MkWithContext context content;

    instance Functor (WithContext context) where
    {
        fmap ab (MkWithContext context a) = MkWithContext context (ab a);
    };

    instance Foldable (WithContext context) where
    {
        foldMap am (MkWithContext _ a) = am a;
    };

    instance Traversable (WithContext context) where
    {
        traverse afb (MkWithContext context a) = fmap (MkWithContext context) (afb a);
        sequenceA (MkWithContext context fa) = fmap (MkWithContext context) fa;
    };

    instance Comonad (WithContext context) where
    {
        extract (MkWithContext _ content) = content;
        extend wab wa@(MkWithContext context _) = MkWithContext context $ wab wa;
    };

    instance (HasNewValue context,HasNewValue content) => HasNewValue (WithContext context content) where
    {
        newValue = MkWithContext newValue newValue;
    };

    data WithContextSelector (editx :: *) (editn :: *) (edit :: *) where
    {
        EditContext :: WithContextSelector editx editn editx;
        EditContent :: WithContextSelector editx editn editn;
    };

    instance TestEquality (WithContextSelector ea eb) where
    {
        testEquality EditContext EditContext = Just Refl;
        testEquality EditContent EditContent = Just Refl;
        testEquality _ _ = Nothing;
    };

    instance (SubjectReader (EditReader editx),SubjectReader (EditReader editn)) => SubjectTupleSelector (WithContextSelector editx editn) where
    {
        type TupleSubject (WithContextSelector editx editn) = WithContext (EditSubject editx) (EditSubject editn);
        tupleReadFromSubject EditContext (MkWithContext x _n) = x;
        tupleReadFromSubject EditContent (MkWithContext _x n) = n;
    };

    instance FiniteTupleSelector (WithContextSelector ex en) where
    {
        tupleConstruct f = MkWithContext <$> f EditContext <*> f EditContent;
    };

    instance (c (EditReader ex),c (EditReader en)) => TupleReaderWitness c (WithContextSelector ex en) where
    {
        tupleReaderWitness _ EditContext = MkConstraintWitness;
        tupleReaderWitness _ EditContent = MkConstraintWitness;
    };

    instance (c ex,c en) => TupleWitness c (WithContextSelector ex en) where
    {
        tupleWitness _ EditContext = MkConstraintWitness;
        tupleWitness _ EditContent = MkConstraintWitness;
    };

    contextEditLens :: PureEditLens (TupleEdit (WithContextSelector editx editn)) editx;
    contextEditLens = tupleEditLens EditContext;
    contentEditLens :: PureEditLens (TupleEdit (WithContextSelector editx editn)) editn;
    contentEditLens = tupleEditLens EditContent;

    contextGeneralLens :: GeneralLens (TupleEdit (WithContextSelector editx editn)) editx;
    contextGeneralLens = tupleGeneralLens EditContext;
    contentGeneralLens :: GeneralLens (TupleEdit (WithContextSelector editx editn)) editn;
    contentGeneralLens = tupleGeneralLens EditContent;

    type ContextEditReader x n = TupleEditReader (WithContextSelector x n);
    type ContextEdit x n = TupleEdit (WithContextSelector x n);

    contextualiseReadFunction :: forall edita editb. ReadFunction (EditReader edita) (EditReader editb) -> ReadFunction (EditReader edita) (ContextEditReader edita editb);
    contextualiseReadFunction _rf (MkTupleEditReader EditContext rt) = readable rt;
    contextualiseReadFunction rf (MkTupleEditReader EditContent rt) = rf rt;

    contextualiseEditFunction :: forall state edita editb. EditFunction state edita editb -> EditFunction state edita (ContextEdit edita editb);
    contextualiseEditFunction (MkEditFunction i g u) = let
    {
        g' :: state -> ReadFunction (EditReader edita) (ContextEditReader edita editb);
        g' curstate = contextualiseReadFunction @edita @editb $ g curstate;

        u' :: edita -> state -> Readable (EditReader edita) (state,[ContextEdit edita editb]);
        u' ea oldstate = do
        {
            (newstate,ebs) <- u ea oldstate;
            return (newstate,(MkTupleEdit EditContext ea):(fmap (MkTupleEdit EditContent) ebs));
        };
    } in MkEditFunction i g' u';

    contextualiseEditLens :: EditLens state edita editb -> EditLens state edita (ContextEdit edita editb);
    contextualiseEditLens (MkEditLens f pe) = let
    {
        f' = contextualiseEditFunction f;
        pe' oldstate (MkTupleEdit EditContext ea) = return $ pure (oldstate,[ea]);
        pe' oldstate (MkTupleEdit EditContent eb) = pe oldstate eb;
    } in MkEditLens f' pe';

    contextualiseGeneralLens :: GeneralLens edita editb -> GeneralLens edita (ContextEdit edita editb);
    contextualiseGeneralLens (MkCloseState lens) = MkCloseState $ contextualiseEditLens lens;

    contextJoinEditFunctions :: forall s1 s2 edita editb1 editb2.
        EditFunction s1 edita editb1 -> EditFunction s2 edita editb2 -> EditFunction (s1,s2) edita (ContextEdit editb1 editb2);
    contextJoinEditFunctions ef1 ef2 = MkEditFunction
    {
        editAccess = pairStateAccess (editAccess ef1) (editAccess ef2),
        editGet = \(cur1,cur2) -> \case
        {
            MkTupleEditReader EditContext rt -> editGet ef1 cur1 rt;
            MkTupleEditReader EditContent rt -> editGet ef2 cur2 rt;
        },
        editUpdate = \ea (old1,old2) -> do
        {
            (new1,eb1s) <- editUpdate ef1 ea old1;
            (new2,eb2s) <- editUpdate ef2 ea old2;
            return ((new1,new2),fmap (MkTupleEdit EditContext) eb1s ++ fmap (MkTupleEdit EditContent) eb2s);
        }
    };

    contextJoinEditLenses :: forall s1 s2 edita editb1 editb2.
        EditLens s1 edita editb1 -> EditLens s2 edita editb2 -> EditLens (s1,s2) edita (ContextEdit editb1 editb2);
    contextJoinEditLenses lens1 lens2 = MkEditLens
    {
        editLensFunction = contextJoinEditFunctions (editLensFunction lens1) (editLensFunction lens2),
        editLensPutEdit = \(old1,old2) -> \case
        {
            MkTupleEdit EditContext editb -> do
            {
                fseas <- editLensPutEdit lens1 old1 editb;
                return $ fmap (\(new1,eas) -> ((new1,old2),eas)) fseas;
            };
            MkTupleEdit EditContent editb -> do
            {
                fseas <- editLensPutEdit lens2 old2 editb;
                return $ fmap (\(new2,eas) -> ((old1,new2),eas)) fseas;
            };
        }
    };

    nullContextGeneralLens :: Edit edit => GeneralLens edit (ContextEdit UnitEdit edit);
    nullContextGeneralLens = MkCloseState $ contextJoinEditLenses unitLens identityState;

    liftContextReadFunction :: forall edita editb editx. ReadFunction (EditReader edita) (EditReader editb) -> ReadFunction (ContextEditReader editx edita) (ContextEditReader editx editb);
    liftContextReadFunction _ (MkTupleEditReader EditContext reader) = mapReadable (tupleReadFunction EditContext) $ readable reader;
    liftContextReadFunction rf (MkTupleEditReader EditContent reader) = mapReadable (tupleReadFunction EditContent) $ rf reader;

    liftContextEditFunction :: forall state edita editb editx. EditFunction state edita editb -> EditFunction state (ContextEdit editx edita) (ContextEdit editx editb);
    liftContextEditFunction (MkEditFunction i g u) = let
    {
        g' :: state -> ContextEditReader editx editb t -> Readable (ContextEditReader editx edita) t;
        g' cur = liftContextReadFunction $ g cur;

        u' :: ContextEdit editx edita -> state -> Readable (ContextEditReader editx edita) (state, [ContextEdit editx editb]);
        u' (MkTupleEdit EditContext edit) old = return (old,[MkTupleEdit EditContext edit]);
        u' (MkTupleEdit EditContent edit) old = do
        {
            (new,edits) <- mapReadable (tupleReadFunction EditContent) $ u edit old;
            return (new,fmap (MkTupleEdit EditContent) edits);
        };
    } in MkEditFunction i g' u';

    liftContextEditLens :: forall state edita editb editx. EditLens state edita editb -> EditLens state (ContextEdit editx edita) (ContextEdit editx editb);
    liftContextEditLens (MkEditLens (ef :: EditFunction state edita editb) pe) = let
    {
        ef' = liftContextEditFunction ef;

        pe' :: state -> ContextEdit editx editb -> Readable (ContextEditReader editx edita) (Maybe (state, [ContextEdit editx edita]));
        pe' old (MkTupleEdit EditContext edit) = return $ pure (old,[MkTupleEdit EditContext edit]);
        pe' old (MkTupleEdit EditContent edit) = do
        {
            mnewedits <- mapReadable (tupleReadFunction EditContent) $ pe old edit;
            for mnewedits $ \(new,edits) -> return (new,fmap (MkTupleEdit EditContent) edits);
        };
    } in MkEditLens ef' pe';

    liftContextGeneralLens :: forall edita editb editx. GeneralLens edita editb -> GeneralLens (ContextEdit editx edita) (ContextEdit editx editb);
    liftContextGeneralLens (MkCloseState lens) = MkCloseState $ liftContextEditLens lens;

    liftContentReadFunction :: forall edita editb editn. ReadFunction (EditReader edita) (EditReader editb) -> ReadFunction (ContextEditReader edita editn) (ContextEditReader editb editn);
    liftContentReadFunction _ (MkTupleEditReader EditContent reader) = mapReadable (tupleReadFunction EditContent) $ readable reader;
    liftContentReadFunction rf (MkTupleEditReader EditContext reader) = mapReadable (tupleReadFunction EditContext) $ rf reader;

    liftContentEditFunction :: forall state edita editb editn. EditFunction state edita editb -> EditFunction state (ContextEdit edita editn) (ContextEdit editb editn);
    liftContentEditFunction (MkEditFunction i g u) = let
    {
        g' :: state -> ContextEditReader editb editn t -> Readable (ContextEditReader edita editn) t;
        g' cur = liftContentReadFunction $ g cur;

        u' :: ContextEdit edita editn -> state -> Readable (ContextEditReader edita editn) (state, [ContextEdit editb editn]);
        u' (MkTupleEdit EditContent edit) old = return (old,[MkTupleEdit EditContent edit]);
        u' (MkTupleEdit EditContext edit) old = do
        {
            (new,edits) <- mapReadable (tupleReadFunction EditContext) $ u edit old;
            return (new,fmap (MkTupleEdit EditContext) edits);
        };
    } in MkEditFunction i g' u';

    liftContentEditLens :: forall state edita editb editn. EditLens state edita editb -> EditLens state (ContextEdit edita editn) (ContextEdit editb editn);
    liftContentEditLens (MkEditLens (ef :: EditFunction state edita editb) pe) = let
    {
        ef' = liftContentEditFunction ef;

        pe' :: state -> ContextEdit editb editn -> Readable (ContextEditReader edita editn) (Maybe (state, [ContextEdit edita editn]));
        pe' old (MkTupleEdit EditContent edit) = return $ pure (old,[MkTupleEdit EditContent edit]);
        pe' old (MkTupleEdit EditContext edit) = do
        {
            mnewedits <- mapReadable (tupleReadFunction EditContext) $ pe old edit;
            for mnewedits $ \(new,edits) -> return (new,fmap (MkTupleEdit EditContext) edits);
        };
    } in MkEditLens ef' pe';

    liftContentGeneralLens :: forall edita editb editn. GeneralLens edita editb -> GeneralLens (ContextEdit edita editn) (ContextEdit editb editn);
    liftContentGeneralLens (MkCloseState lens) = MkCloseState $ liftContentEditLens lens;

    carryContextGeneralLens :: (Edit editx,Edit edita,Edit editb) =>
        GeneralLens (ContextEdit editx edita) editb -> GeneralLens (ContextEdit editx edita) (ContextEdit editx editb);
    carryContextGeneralLens lens = liftContentGeneralLens (tupleGeneralLens EditContext) <.> contextualiseGeneralLens lens;
}
