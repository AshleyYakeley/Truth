module Truth.Core.Types.PointedEditLens where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Tuple;
    import Truth.Core.Types.Pair;
    import Truth.Core.Types.Context;
    import Truth.Core.Types.Lattice;


    data PointedEditFunction c editp edita editb = forall state. MkPointedEditFunction (EditFunction c state (ContextEdit editp edita) editb);

    data PointedEditLens c f editp edita editb = forall state. MkPointedEditLens (EditLens' c f state (ContextEdit editp edita) editb);

    instance (ReadableConstraint c,Monad f,Traversable f,Edit editp) => ConstrainedCategory (PointedEditLens c f editp) where
    {
        type CategoryConstraint (PointedEditLens c f editp) edit = Edit edit;
        cid = let
        {
            editInitial = ();

            editGet :: () -> ReadFunction c (ContextEditReader editp edit) (EditReader edit);
            editGet () rt = readable $ MkTupleEditReader EditContent rt;

            editUpdate :: ContextEdit editp edit -> () -> Readable c (ContextEditReader editp edit) ((),[edit]);
            editUpdate (MkTupleEdit EditContext _) () = pure $ pure [];
            editUpdate (MkTupleEdit EditContent edit) () = pure $ pure $ pure edit;

            editLensFunction = MkEditFunction{..};

            editLensPutEdit :: () -> edit -> Readable c (ContextEditReader editp edit) (f ((),[ContextEdit editp edit]));
            editLensPutEdit () edit = pure $ pure $ pure $ pure $ MkTupleEdit EditContent edit;
        } in MkPointedEditLens $ MkEditLens{..};


        (MkPointedEditLens (lensBC :: EditLens' c f stateBC (ContextEdit editp editb) editc)) <.> (MkPointedEditLens (lensAB :: EditLens' c f stateAB (ContextEdit editp edita) editb)) = let
        {
            funcAB = editLensFunction lensAB;
            funcBC = editLensFunction lensBC;
            convAB :: stateAB -> ReadFunction c (ContextEditReader editp edita) (ContextEditReader editp editb);
            convAB _ (MkTupleEditReader EditContext rt) = readable $ MkTupleEditReader EditContext rt;
            convAB curAB (MkTupleEditReader EditContent rt) = editGet funcAB curAB rt;
        } in MkPointedEditLens $ MkEditLens
        {
            editLensFunction = MkEditFunction
            {
                editInitial = (editInitial funcBC,editInitial funcAB),
                editGet = \(curBC,curAB) readC -> mapGenReadable (convAB curAB) $ editGet funcBC curBC readC,
                editUpdate = \editpa (oldBC,oldAB) -> do
                {
                    (newAB,editBs) <- editUpdate funcAB editpa oldAB;
                    (midBC,peditCs) <- case editpa of
                    {
                        MkTupleEdit EditContext editP -> mapGenReadable (convAB oldAB) $ editUpdate funcBC (MkTupleEdit EditContext editP) oldBC;
                        MkTupleEdit EditContent _ -> return (oldBC,[]);
                    };
                    (newBC,editCs) <- mapGenReadable (convAB oldAB) $ editUpdates funcBC (fmap (MkTupleEdit EditContent) editBs) midBC;
                    return ((newBC,newAB),peditCs ++ editCs);
                }
            },
            editLensPutEdit = \(oldBC,oldAB) editC -> do
            {
                fslb <- mapGenReadable (convAB oldAB) $ editLensPutEdit lensBC oldBC editC;
                ff <- for fslb $ \(newBC,editPBs) -> let
                {
                    editPsBs :: forall t. WithContextSelector editp editb t -> [t];
                    editPsBs = getAllF $ splitTupleEditList editPBs;
                    editPs = editPsBs EditContext;
                    editBs = editPsBs EditContent;
                } in do
                {
                    fsla <- editLensPutEdits lensAB oldAB $ editBs;
                    return $ fmap (\(newAB,editPAs) -> ((newBC,newAB),(fmap (MkTupleEdit EditContext) editPs) ++ editPAs)) fsla;
                };
                return $ ff >>= id;
            }
        };
    };

    pointedEditLensFunction :: PointedEditLens c f editp edita editb -> PointedEditFunction c editp edita editb;
    pointedEditLensFunction (MkPointedEditLens lens) = MkPointedEditFunction $ editLensFunction lens;

    readOnlyPointedEditLens :: PointedEditFunction c editp edita editb -> PointedEditLens c Maybe editp edita editb;
    readOnlyPointedEditLens (MkPointedEditFunction f) = MkPointedEditLens $ readOnlyEditLens f;

    editLensToPointed :: (ReadableConstraint c, MonadOne f, Edit editp, Edit edita, Edit editb) =>
        EditLens' c f state edita editb -> PointedEditLens c f editp edita editb;
    editLensToPointed lens = case cid of
    {
        MkPointedEditLens idlens -> MkPointedEditLens $ composeState lens idlens;
    };

    composeEditLensPointed :: (ReadableConstraint c,MonadOne f,Edit editp,Edit edita,Edit editb,Edit editc) =>
        EditLens' c f state editb editc -> PointedEditLens c f editp edita editb -> PointedEditLens c f editp edita editc;
    composeEditLensPointed lensBC (MkPointedEditLens lensAB) = MkPointedEditLens $ composeState lensBC lensAB;

    instance (ReadableConstraint c,JoinSemiLatticeEdit editb,Edit editp,Edit edita,Edit editb) =>
        JoinSemiLattice (PointedEditFunction c editp edita editb) where
    {
        (MkPointedEditFunction f1) \/ (MkPointedEditFunction f2) = MkPointedEditFunction $  composeState joinEditFunction $ pairJoinEditFunctions f1 f2;
    };

    instance (ReadableConstraint c,MeetSemiLatticeEdit editb,Edit editp,Edit edita,Edit editb) =>
        MeetSemiLattice (PointedEditFunction c editp edita editb) where
    {
        (MkPointedEditFunction f1) /\ (MkPointedEditFunction f2) = MkPointedEditFunction $  composeState meetEditFunction $ pairJoinEditFunctions f1 f2;
    };
}
