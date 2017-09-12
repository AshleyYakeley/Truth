module Truth.Core.Edit.EditLens where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit.Edit;
    import Truth.Core.Edit.FullEdit;
    import Truth.Core.Edit.EditFunction;


    data EditLens' m state edita editb = MkEditLens
    {
        editLensFunction :: EditFunction state edita editb,
        editLensPutEdit :: state -> editb -> Readable (EditReader edita) (m (state,[edita]))
    };

    editLensPutEdits :: (Monad m,Traversable m,Edit edita) => EditLens' m state edita editb -> state -> [editb] -> Readable (EditReader edita) (m (state,[edita]));
    editLensPutEdits _ oldstate [] = return $ pure $ (oldstate,[]);
    editLensPutEdits lens oldstate (e:ee) = getCompose $ do
    {
        (midstate,ea) <- Compose $ editLensPutEdit lens oldstate e;
        Compose $ mapReadable (applyEdits ea) $ getCompose $ do
        {
            (newstate,eea) <- Compose $ editLensPutEdits lens midstate ee;
            return (newstate,ea ++ eea);
        };
    };

    editLensAllowed :: (MonadOne m) =>
     EditLens' m state edita editb -> state -> editb -> Readable (EditReader edita) Bool;
    editLensAllowed lens st editb = do
    {
        medita <- editLensPutEdit lens st editb;
        return (isJust (getMaybeOne medita));
    };

    type EditLens = EditLens' Maybe;

    type ObjectLens = EditLens ();
{-
    instance IsBiMap (EditLens' state) where
    {
        mapBiMapM ff felens = MkEditLens
        {
            editLensFunction = editLensFunction felens,
            editLensPutEdit = \state edit -> fmap ff (editLensPutEdit felens state edit)
        };
    };
-}

    instance (MonadOne m) => ConstrainedCategory (EditLens' m ()) where
    {
        type CategoryConstraint (EditLens' m ()) t = Edit t;
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

    instance (MonadOne m) => StateCategory (EditLens' m) where
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

    readOnlyEditLens :: forall state edita editb. EditFunction state edita editb -> EditLens' Maybe state edita editb;
    readOnlyEditLens editLensFunction = let
    {
        editLensPutEdit _ _ = pure Nothing;
    } in MkEditLens{..};

    constEditLens :: forall edita editb. Reader (EditReader editb) => EditSubject editb -> EditLens' Maybe () edita editb;
    constEditLens b = readOnlyEditLens $ constEditFunction b;

    convertEditLens :: forall m edita editb. (Applicative m,EditSubject edita ~ EditSubject editb,FullEdit edita,FullEdit editb) =>
        EditLens' m () edita editb;
    convertEditLens = let
    {
        editLensFunction :: EditFunction () edita editb;
        editLensFunction = convertEditFunction;
        editLensPutEdit :: () -> editb -> Readable (EditReader edita) (m ((),[edita]));
        editLensPutEdit () editb = do
        {
            newsubject <- fromReadFunctionM (applyEdit editb) fromReader;
            editbs <- getReplaceEditsM newsubject;
            return $ pure $ ((),editbs);
        };
    } in MkEditLens{..};

    invertEditLens :: (state -> ReadFunction (EditReader editb) (EditReader edita)) -> EditLens' Identity state edita editb -> EditLens' Identity state editb edita;
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
