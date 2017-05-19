module Truth.Core.Object.MutableEdit where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Pair;


    data MutableEdit m edit userstate = MkMutableEdit
    {
        mutableRead :: MutableRead m (EditReader edit),
        mutableEdit :: [edit] -> m (Maybe (m userstate))
    };

    mutableReadEdit :: Monad m => MutableEdit m edit userstate -> Readable (EditReader edit) [edit] -> m (Maybe (m userstate));
    mutableReadEdit MkMutableEdit{..}  MkReadable{..} = do
    {
        edits <- unReadable mutableRead;
        mutableEdit edits;
    };

    mutableAllowed :: Functor m => MutableEdit m edit userstate -> edit -> m Bool;
    mutableAllowed me edit = mutableAlloweds me [edit];

    mutableAlloweds :: Functor m => MutableEdit m edit userstate -> [edit] -> m Bool;
    mutableAlloweds me edits = fmap isJust $ mutableEdit me edits;

    singleMutableEdit :: Applicative m => (edit -> m ()) -> [edit] -> m (Maybe (m ()));
    singleMutableEdit mutableEdit' edits = pure $ Just $ for_ edits mutableEdit';

    instance Functor m => Functor (MutableEdit m edit) where
    {
        fmap ab (MkMutableEdit r e) = MkMutableEdit r $ fmap (fmap (fmap (fmap ab))) e;
    };

    remonadMutableEdit :: Functor m1 => (forall a. m1 a -> m2 a) -> MutableEdit m1 edit userstate -> MutableEdit m2 edit userstate;
    remonadMutableEdit mf (MkMutableEdit r e) = MkMutableEdit (fmap mf r) (fmap (mf . (fmap (fmap mf))) e);

    pairMutableEdit :: Applicative m => MutableEdit m ea ua -> MutableEdit m eb ub -> MutableEdit m (PairEdit ea eb) (ua,ub);
    pairMutableEdit mea meb = MkMutableEdit
    {
        mutableRead = pairMutableRead (mutableRead mea) (mutableRead meb),
        mutableEdit = \edits -> let
        {
            (eas,ebs) = partitionPairEdits edits;
        } in liftA2 (liftA2 (liftA2 (,))) (mutableEdit mea eas) (mutableEdit meb ebs)
    };
}
