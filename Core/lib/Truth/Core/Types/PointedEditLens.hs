module Truth.Core.Types.PointedEditLens where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Tuple;
    import Truth.Core.Types.Pair;
    import Truth.Core.Types.Context;
    import Truth.Core.Types.Lattice;


    data PointedEditFunction editp edita editb = MkPointedEditFunction (EditFunction () (ContextEdit editp edita) editb);

    data PointedEditLens f editp edita editb = MkPointedEditLens (EditLens' f () (ContextEdit editp edita) editb);

    instance (Monad f,Traversable f,Edit editp) => ConstrainedCategory (PointedEditLens f editp) where
    {
        type CategoryConstraint (PointedEditLens f editp) edit = Edit edit;
        cid = let
        {
            editInitial = ();

            editGet :: () -> ReadFunction (ContextEditReader editp edit) (EditReader edit);
            editGet () rt = readable $ MkTupleEditReader EditContent rt;

            editUpdate :: ContextEdit editp edit -> () -> Readable (ContextEditReader editp edit) ((),[edit]);
            editUpdate (MkTupleEdit EditContext _) () = pure $ pure [];
            editUpdate (MkTupleEdit EditContent edit) () = pure $ pure $ pure edit;

            editLensFunction = MkEditFunction{..};

            editLensPutEdit :: () -> edit -> Readable (ContextEditReader editp edit) (f ((),[ContextEdit editp edit]));
            editLensPutEdit () edit = pure $ pure $ pure $ pure $ MkTupleEdit EditContent edit;
        } in MkPointedEditLens $ MkEditLens{..};


        (MkPointedEditLens (lensBC :: EditLens' f () (ContextEdit editp editb) editc)) <.> (MkPointedEditLens (lensAB :: EditLens' f () (ContextEdit editp edita) editb)) = let
        {
            funcAB = editLensFunction lensAB;
            funcBC = editLensFunction lensBC;
            convAB :: () -> ReadFunction (ContextEditReader editp edita) (ContextEditReader editp editb);
            convAB () (MkTupleEditReader EditContext rt) = readable $ MkTupleEditReader EditContext rt;
            convAB () (MkTupleEditReader EditContent rt) = editGet funcAB () rt;
        } in MkPointedEditLens $ MkEditLens
        {
            editLensFunction = MkEditFunction
            {
                editInitial = (),
                editGet = \() readC -> mapGenReadable (convAB ()) $ editGet funcBC () readC,
                editUpdate = \editpa () -> do
                {
                    ((),editBs) <- editUpdate funcAB editpa ();
                    (midBC,peditCs) <- case editpa of
                    {
                        MkTupleEdit EditContext editP -> mapGenReadable (convAB ()) $ editUpdate funcBC (MkTupleEdit EditContext editP) ();
                        MkTupleEdit EditContent _ -> return ((),[]);
                    };
                    ((),editCs) <- mapGenReadable (convAB ()) $ editUpdates funcBC (fmap (MkTupleEdit EditContent) editBs) midBC;
                    return ((),peditCs ++ editCs);
                }
            },
            editLensPutEdit = \() editC -> do
            {
                fslb <- mapGenReadable (convAB ()) $ editLensPutEdit lensBC () editC;
                ff <- for fslb $ \((),editPBs) -> let
                {
                    editPsBs :: forall t. WithContextSelector editp editb t -> [t];
                    editPsBs = getAllF $ splitTupleEditList editPBs;
                    editPs = editPsBs EditContext;
                    editBs = editPsBs EditContent;
                } in do
                {
                    fsla <- editLensPutEdits lensAB () $ editBs;
                    return $ fmap (\((),editPAs) -> ((),(fmap (MkTupleEdit EditContext) editPs) ++ editPAs)) fsla;
                };
                return $ ff >>= id;
            }
        };
    };

    pointedEditLensFunction :: PointedEditLens f editp edita editb -> PointedEditFunction editp edita editb;
    pointedEditLensFunction (MkPointedEditLens lens) = MkPointedEditFunction $ editLensFunction lens;

    readOnlyPointedEditLens :: PointedEditFunction editp edita editb -> PointedEditLens Maybe editp edita editb;
    readOnlyPointedEditLens (MkPointedEditFunction f) = MkPointedEditLens $ readOnlyEditLens f;

    editLensToPointed :: (MonadOne f, Edit editp, Edit edita, Edit editb) =>
        EditLens' f () edita editb -> PointedEditLens f editp edita editb;
    editLensToPointed lens = case cid of
    {
        MkPointedEditLens idlens -> MkPointedEditLens $ lens <.> idlens;
    };

    composeEditLensPointed :: (MonadOne f,Edit editp,Edit edita,Edit editb,Edit editc) =>
        EditLens' f () editb editc -> PointedEditLens f editp edita editb -> PointedEditLens f editp edita editc;
    composeEditLensPointed lensBC (MkPointedEditLens lensAB) = MkPointedEditLens $ lensBC <.> lensAB;

    instance (JoinSemiLatticeEdit editb,Edit editp,Edit edita,Edit editb) =>
        JoinSemiLattice (PointedEditFunction editp edita editb) where
    {
        (MkPointedEditFunction f1) \/ (MkPointedEditFunction f2) = MkPointedEditFunction $ joinEditFunction <.> editToObjectFunction (pairJoinEditFunctions f1 f2);
    };

    instance (MeetSemiLatticeEdit editb,Edit editp,Edit edita,Edit editb) =>
        MeetSemiLattice (PointedEditFunction editp edita editb) where
    {
        (MkPointedEditFunction f1) /\ (MkPointedEditFunction f2) = MkPointedEditFunction $ meetEditFunction <.> editToObjectFunction (pairJoinEditFunctions f1 f2);
    };

    carryPointedEditLens :: (Edit editx,Edit edita,Edit editb) =>
        PointedEditLens Maybe editx edita editb -> GeneralLens (ContextEdit editx edita) (ContextEdit editx editb);
    carryPointedEditLens (MkPointedEditLens lens) = liftContentGeneralLens (tupleEditLens EditContext) <.> MkCloseState (contextualiseEditLens lens);
}
