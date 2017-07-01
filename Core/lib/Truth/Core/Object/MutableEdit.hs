module Truth.Core.Object.MutableEdit where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Pair;


    data MutableEdit m edit = MkMutableEdit
    {
        mutableRead :: MutableRead m (EditReader edit),
        mutableEdit :: [edit] -> m (Maybe (m ()))
    };

    mutableReadEdit :: Monad m => MutableEdit m edit -> Readable (EditReader edit) [edit] -> m (Maybe (m ()));
    mutableReadEdit MkMutableEdit{..}  MkReadable{..} = do
    {
        edits <- unReadable mutableRead;
        mutableEdit edits;
    };

    mutableAllowed :: Functor m => MutableEdit m edit -> edit -> m Bool;
    mutableAllowed me edit = mutableAlloweds me [edit];

    mutableAlloweds :: Functor m => MutableEdit m edit -> [edit] -> m Bool;
    mutableAlloweds me edits = fmap isJust $ mutableEdit me edits;

    singleMutableEdit :: Applicative m => (edit -> m ()) -> [edit] -> m (Maybe (m ()));
    singleMutableEdit mutableEdit' edits = pure $ Just $ for_ edits mutableEdit';

    remonadMutableEdit :: Functor m1 => (forall a. m1 a -> m2 a) -> MutableEdit m1 edit -> MutableEdit m2 edit;
    remonadMutableEdit mf (MkMutableEdit r e) = MkMutableEdit (fmap mf r) (fmap (mf . (fmap (fmap mf))) e);

    liftMutableEdit :: (MonadTrans t,Monad m) => MutableEdit m edit -> MutableEdit (t m) edit;
    liftMutableEdit = remonadMutableEdit lift;

    pairMutableEdit :: Applicative m => MutableEdit m ea -> MutableEdit m eb -> MutableEdit m (PairEdit ea eb);
    pairMutableEdit mea meb = MkMutableEdit
    {
        mutableRead = pairMutableRead (mutableRead mea) (mutableRead meb),
        mutableEdit = \edits -> let
        {
            (eas,ebs) = partitionPairEdits edits;
        } in liftA2 (liftA2 (liftA2 mappend)) (mutableEdit mea eas) (mutableEdit meb ebs)
    };
}
