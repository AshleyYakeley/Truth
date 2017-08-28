module Truth.Core.Types.Context where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Tuple;


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

    $(return []);
    instance HasTypeInfo WithContext where
    {
        typeWitness = $(generateWitness [t|WithContext|]);
        typeName _ = "WithContext";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance Functor (WithContext context);
            instance Foldable (WithContext context);
            instance Traversable (WithContext context);
            instance Comonad (WithContext context);
            instance (HasNewValue context,HasNewValue content) => HasNewValue (WithContext context content);
        |]);
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

    instance (Edit editx,Edit editn) => TupleSelector (WithContextSelector editx editn) where
    {
        type TupleSubject (WithContextSelector editx editn) = WithContext (EditSubject editx) (EditSubject editn);
        tupleReadFrom EditContext (MkWithContext x _n) = x;
        tupleReadFrom EditContent (MkWithContext _x n) = n;
    };

    instance (Edit ex,Edit en) => FiniteTupleSelector (WithContextSelector ex en) where
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

    instance (Edit ex,HasTypeInfo ex,Edit en,HasTypeInfo en) => TupleHasInfo (WithContextSelector ex en) where
    {
        tupleHasInfo EditContext = typeInfo;
        tupleHasInfo EditContent = typeInfo;
    };

    $(return []);
    instance HasTypeInfo WithContextSelector where
    {
        typeWitness = $(generateWitness [t|WithContextSelector|]);
        typeName _ = "WithContextSelector";
        typeKnowledge _ = $(generateTypeKnowledge [d|
            instance (TestEquality :: (* -> *) -> Constraint) (WithContextSelector ea eb);
            instance (Edit editx,Edit editn) =>
                TupleSelector (WithContextSelector editx editn) where
            {
                type TupleSubject (WithContextSelector editx editn) = WithContext (EditSubject editx) (EditSubject editn);
            };
            instance (Edit ex,Edit en) => FiniteTupleSelector (WithContextSelector ex en);
            instance (c (EditReader ex),c (EditReader en)) => TupleReaderWitness c (WithContextSelector ex en);
            instance (c ex,c en) => TupleWitness c (WithContextSelector ex en);
        |]);
    };

    contextLens :: Applicative m => EditLens' c m () (TupleEdit (WithContextSelector editx editn)) editx;
    contextLens = tupleEditLens EditContext;
    contentLens :: Applicative m => EditLens' c m () (TupleEdit (WithContextSelector editx editn)) editn;
    contentLens = tupleEditLens EditContent;

    type ContextEditReader x n = TupleEditReader (WithContextSelector x n);
    type ContextEdit x n = TupleEdit (WithContextSelector x n);

    contextualiseReadFunction :: forall c edita editb. ReadFunction c (EditReader edita) (EditReader editb) -> ReadFunction c (EditReader edita) (ContextEditReader edita editb);
    contextualiseReadFunction _rf (MkTupleEditReader EditContext rt) = readable rt;
    contextualiseReadFunction rf (MkTupleEditReader EditContent rt) = rf rt;

    contextualiseEditFunction :: forall c state edita editb. EditFunction c state edita editb -> EditFunction c state edita (ContextEdit edita editb);
    contextualiseEditFunction (MkEditFunction i g u) = let
    {
        g' :: state -> ReadFunction c (EditReader edita) (ContextEditReader edita editb);
        g' curstate = contextualiseReadFunction @c @edita @editb $ g curstate;

        u' :: edita -> state -> Readable c (EditReader edita) (state,[ContextEdit edita editb]);
        u' ea oldstate = do
        {
            (newstate,ebs) <- u ea oldstate;
            return (newstate,(MkTupleEdit EditContext ea):(fmap (MkTupleEdit EditContent) ebs));
        };
    } in MkEditFunction i g' u';

    contextualiseEditLens :: Applicative m => EditLens' c m state edita editb -> EditLens' c m state edita (ContextEdit edita editb);
    contextualiseEditLens (MkEditLens f pe) = let
    {
        f' = contextualiseEditFunction f;
        pe' oldstate (MkTupleEdit EditContext ea) = return $ pure (oldstate,[ea]);
        pe' oldstate (MkTupleEdit EditContent eb) = pe oldstate eb;
    } in MkEditLens f' pe';

    contextJoinEditFunctions :: forall c s1 s2 edita editb1 editb2.
        EditFunction c s1 edita editb1 -> EditFunction c s2 edita editb2 -> EditFunction c (s1,s2) edita (ContextEdit editb1 editb2);
    contextJoinEditFunctions ef1 ef2 = MkEditFunction
    {
        editInitial = (editInitial ef1,editInitial ef2),
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

    contextJoinEditLenses :: forall c f s1 s2 edita editb1 editb2. Functor f =>
        EditLens' c f s1 edita editb1 -> EditLens' c f s2 edita editb2 -> EditLens' c f (s1,s2) edita (ContextEdit editb1 editb2);
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
}
