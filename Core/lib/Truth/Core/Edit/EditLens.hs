module Truth.Core.Edit.EditLens where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit.Edit;
    import Truth.Core.Edit.FullEdit;
    import Truth.Core.Edit.EditFunction;


    data EditLens' c m state edita editb = MkEditLens
    {
        editLensFunction :: EditFunction c state edita editb,
        editLensPutEdit :: state -> editb -> Readable c (EditReader edita) (m (state,[edita]))
    };

    type PureEditLens' = EditLens' Monad;
    type IOEditLens' = EditLens' MonadIO;

    pureToEditLens :: PureEditLens' m state edita editb -> EditLens' c m state edita editb;
    pureToEditLens (MkEditLens f pe) = MkEditLens (pureToEditFunction f) (\s eb -> pureToReadable $ pe s eb);

    editLensPutEdits :: (Monad m,Traversable m,Edit edita,ReadableConstraint c) => EditLens' c m state edita editb -> state -> [editb] -> Readable c (EditReader edita) (m (state,[edita]));
    editLensPutEdits _ oldstate [] = return $ pure $ (oldstate,[]);
    editLensPutEdits lens oldstate (e:ee) = getCompose $ do
    {
        (midstate,ea) <- MkCompose $ editLensPutEdit lens oldstate e;
        MkCompose $ mapReadable (applyEdits ea) $ getCompose $ do
        {
            (newstate,eea) <- MkCompose $ editLensPutEdits lens midstate ee;
            return (newstate,ea ++ eea);
        };
    };

    editLensAllowed :: (MonadOne m) =>
     PureEditLens' m state edita editb -> state -> editb -> PureReadable (EditReader edita) Bool;
    editLensAllowed lens st editb = do
    {
        medita <- editLensPutEdit lens st editb;
        return (isJust (getMaybeOne medita));
    };

    type PureEditLens = PureEditLens' Maybe;
{-
    instance IsBiMap (PureEditLens' state) where
    {
        mapBiMapM ff felens = MkEditLens
        {
            editLensFunction = editLensFunction felens,
            editLensPutEdit = \state edit -> fmap ff (editLensPutEdit felens state edit)
        };
    };
-}

    instance (ReadableConstraint c,MonadOne m) => ConstrainedCategory (EditLens' c m ()) where
    {
        type CategoryConstraint (EditLens' c m ()) t = Edit t;
        cid = let
        {
            editLensFunction = cid;
            editLensPutEdit () edit = pure $ pure ((),[edit]);
        } in MkEditLens{..};
        fel2 <.> fel1 = MkEditLens
        {
            editLensFunction = editLensFunction fel2 <.> editLensFunction fel1,
            editLensPutEdit = \() editc -> do
            {
                meditb <- mapGenReadable (editGet (editLensFunction fel1) ()) (editLensPutEdit fel2 () editc);
                case retrieveOne meditb of
                {
                    SuccessResult ((),editbs) -> editLensPutEdits fel1 () editbs;
                    FailureResult (MkLimit mx) -> return mx;
                };
            }
        };
    };

    instance (ReadableConstraint c,MonadOne m) => StateCategory (EditLens' c m) where
    {
        identityState = let
        {
            editLensFunction = identityState;
            editLensPutEdit st edit = pure $ pure (st,[edit]);
        } in MkEditLens{..};
        composeState fel2 fel1 = MkEditLens
        {
            editLensFunction = composeState (editLensFunction fel2) (editLensFunction fel1),
            editLensPutEdit = \(olds1,olds2) editc -> do
            {
                meditb <- mapGenReadable (editGet (editLensFunction fel1) olds1) (editLensPutEdit fel2 olds2 editc);
                case retrieveOne meditb of
                {
                    SuccessResult (news2,editbs) -> do
                    {
                        mn1ea <- editLensPutEdits fel1 olds1 editbs;
                        return $ fmap (\(news1,edita) -> ((news1,news2),edita)) mn1ea;
                    };
                    FailureResult (MkLimit mx) -> return mx;
                };
            }
        };
    };

    convertEditLens :: forall c m edita editb. (ReadableConstraint c,Applicative m,EditSubject edita ~ EditSubject editb,FullEdit c edita,FullEdit c editb) =>
        EditLens' c m () edita editb;
    convertEditLens = let
    {
        editLensFunction :: EditFunction c () edita editb;
        editLensFunction = convertEditFunction;
        editLensPutEdit :: () -> editb -> Readable c (EditReader edita) (m ((),[edita]));
        editLensPutEdit () editb = case selfReadable @c @(EditReader edita) of
        {
            MkConstraintWitness -> do
            {
                newsubject <- fromReadFunctionM @c (pureToReadFunction $ applyEdit editb) (fromReader @c);
                editbs <- getReplaceEditsM @c newsubject;
                return $ pure $ ((),editbs);
            };
        };
    } in MkEditLens{..};

    invertEditLens :: (state -> PureReadFunction (EditReader editb) (EditReader edita)) -> PureEditLens' Identity state edita editb -> PureEditLens' Identity state editb edita;
    invertEditLens srfba lensab = MkEditLens
    {
        editLensFunction = MkEditFunction
        {
            editInitial = editInitial $ editLensFunction lensab,
            editGet = srfba,
            editUpdate = \eb st -> fmap runIdentity $ mapReadable (srfba st) $ editLensPutEdit lensab st eb
        },
        editLensPutEdit = \st ea -> fmap pure $ mapReadable (srfba st) $ editUpdate (editLensFunction lensab) ea st
    };
}
