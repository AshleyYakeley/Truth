module Truth.Core.Types.PointedEditLens where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Tuple;
    import Truth.Core.Types.Pair;


    data PointedEditLens c f editp edita editb = forall state. MkPointedEditLens (EditLens' c f state (PairEdit editp edita) editb);

    instance (ReadableConstraint c,Monad f,Traversable f,Edit editp) => ConstrainedCategory (PointedEditLens c f editp) where
    {
        type CategoryConstraint (PointedEditLens c f editp) edit = Edit edit;
        cid = let
        {
            editInitial = ();

            editGet :: () -> ReadFunction c (PairEditReader editp edit) (EditReader edit);
            editGet () rt = readable $ MkTupleEditReader EditSecond rt;

            editUpdate :: PairEdit editp edit -> () -> Readable c (PairEditReader editp edit) ((),[edit]);
            editUpdate (MkTupleEdit EditFirst _) () = pure $ pure [];
            editUpdate (MkTupleEdit EditSecond edit) () = pure $ pure $ pure edit;

            editLensFunction = MkEditFunction{..};

            editLensPutEdit :: () -> edit -> Readable c (PairEditReader editp edit) (f ((),[PairEdit editp edit]));
            editLensPutEdit () edit = pure $ pure $ pure $ pure $ MkTupleEdit EditSecond edit;
        } in MkPointedEditLens $ MkEditLens{..};


        (MkPointedEditLens (lensBC :: EditLens' c f stateBC (PairEdit editp editb) editc)) <.> (MkPointedEditLens (lensAB :: EditLens' c f stateAB (PairEdit editp edita) editb)) = let
        {
            funcAB = editLensFunction lensAB;
            funcBC = editLensFunction lensBC;
            convAB :: stateAB -> ReadFunction c (PairEditReader editp edita) (PairEditReader editp editb);
            convAB _ (MkTupleEditReader EditFirst rt) = readable $ MkTupleEditReader EditFirst rt;
            convAB curAB (MkTupleEditReader EditSecond rt) = editGet funcAB curAB rt;
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
                        MkTupleEdit EditFirst editP -> mapGenReadable (convAB oldAB) $ editUpdate funcBC (MkTupleEdit EditFirst editP) oldBC;
                        MkTupleEdit EditSecond _ -> return (oldBC,[]);
                    };
                    (newBC,editCs) <- mapGenReadable (convAB oldAB) $ editUpdates funcBC (fmap (MkTupleEdit EditSecond) editBs) midBC;
                    return ((newBC,newAB),peditCs ++ editCs);
                }
            },
            editLensPutEdit = \(oldBC,oldAB) editC -> do
            {
                fslb <- mapGenReadable (convAB oldAB) $ editLensPutEdit lensBC oldBC editC;
                ff <- for fslb $ \(newBC,editPBs) -> let
                {
                    editPsBs :: forall t. PairSelector editp editb t -> [t];
                    editPsBs = getAllF $ splitTupleEditList editPBs;
                    editPs = editPsBs EditFirst;
                    editBs = editPsBs EditSecond;
                } in do
                {
                    fsla <- editLensPutEdits lensAB oldAB $ editBs;
                    return $ fmap (\(newAB,editPAs) -> ((newBC,newAB),(fmap (MkTupleEdit EditFirst) editPs) ++ editPAs)) fsla;
                };
                return $ ff >>= id;
            }
        };
    };

    editLensToPointed :: (ReadableConstraint c, MonadOne f, Edit editp, Edit edita, Edit editb) =>
        EditLens' c f state edita editb -> PointedEditLens c f editp edita editb;
    editLensToPointed lens = case cid of
    {
        MkPointedEditLens idlens -> MkPointedEditLens $ composeState lens idlens;
    };
}
