module Truth.Core.Edit.FloatingEditLens where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit.Edit;
    import Truth.Core.Edit.EditLens;
    import Truth.Core.Edit.FloatingEditFunction;


    data FloatingEditLens' m state edita editb = MkFloatingEditLens
    {
        floatingEditLensFunction :: FloatingEditFunction state edita editb,
        floatingEditLensPutEdit :: state -> editb -> Readable (EditReader edita) (m (state,[edita]))
    };

    floatingEditLensPutEdits :: (Monad m,Traversable m) => FloatingEditLens' m state edita editb -> state -> [editb] -> Readable (EditReader edita) (m (state,[edita]));
    floatingEditLensPutEdits _ oldstate [] = return $ pure $ (oldstate,[]);
    floatingEditLensPutEdits lens oldstate (e:ee) = getCompose $ do
    {
        (midstate,ea) <- MkCompose $ floatingEditLensPutEdit lens oldstate e;
        (newstate,eea) <- MkCompose $ floatingEditLensPutEdits lens midstate ee;
        return (newstate,ea ++ eea);
    };

    floatingEditLensAllowed :: (MonadOne m) =>
     FloatingEditLens' m state edita editb -> state -> editb -> Readable (EditReader edita) Bool;
    floatingEditLensAllowed lens state editb = do
    {
        medita <- floatingEditLensPutEdit lens state editb;
        return (isJust (getMaybeOne medita));
    };

    type FloatingEditLens = FloatingEditLens' Maybe;
{-
    instance IsBiMap (FloatingEditLens' state) where
    {
        mapBiMapM ff felens = MkFloatingEditLens
        {
            floatingEditLensFunction = floatingEditLensFunction felens,
            floatingEditLensPutEdit = \state edit -> fmap ff (floatingEditLensPutEdit felens state edit)
        };
    };
-}
    fixedFloatingEditLens :: Functor m => EditLens' m edita editb -> FloatingEditLens' m () edita editb;
    fixedFloatingEditLens lens = MkFloatingEditLens
    {
        floatingEditLensFunction = fixedFloatingEditFunction (editLensFunction lens),
        floatingEditLensPutEdit = \state edit -> fmap (fmap ((,) state)) $ editLensPutEdit lens edit
    };

    instance (Applicative m,MonadOne m) => FloatingMap (FloatingEditLens' m) where
    {
        identityFloating = fixedFloatingEditLens id;
        composeFloating fel2 fel1 = MkFloatingEditLens
        {
            floatingEditLensFunction = composeFloating (floatingEditLensFunction fel2) (floatingEditLensFunction fel1),
            floatingEditLensPutEdit = \(olds1,olds2) editc -> do
            {
                meditb <- mapReadable (floatingEditGet (floatingEditLensFunction fel1) olds1) (floatingEditLensPutEdit fel2 olds2 editc);
                case retrieveOne meditb of
                {
                    SuccessResult (news2,editbs) -> do
                    {
                        mn1ea <- floatingEditLensPutEdits fel1 olds1 editbs;
                        return $ fmap (\(news1,edita) -> ((news1,news2),edita)) mn1ea;
                    };
                    FailureResult (MkLimit mx) -> return mx;
                };
            }
        };
    };

    invertFloatingEditLens :: (state -> ReadFunction (EditReader editb) (EditReader edita)) -> FloatingEditLens' Identity state edita editb -> FloatingEditLens' Identity state editb edita;
    invertFloatingEditLens srfba lensab = MkFloatingEditLens
    {
        floatingEditLensFunction = MkFloatingEditFunction
        {
            floatingEditInitial = floatingEditInitial $ floatingEditLensFunction lensab,
            floatingEditGet = srfba,
            floatingEditUpdate = \eb state -> fmap runIdentity $ mapReadable (srfba state) $ floatingEditLensPutEdit lensab state eb
        },
        floatingEditLensPutEdit = \state ea -> fmap pure $ mapReadable (srfba state) $ floatingEditUpdate (floatingEditLensFunction lensab) ea state
    };
}
