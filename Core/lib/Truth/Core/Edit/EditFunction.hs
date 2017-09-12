module Truth.Core.Edit.EditFunction  where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit.Edit;
    import Truth.Core.Edit.FullEdit;


    data EditFunction c state edita editb = MkEditFunction
    {
        editInitial :: state,
        editGet :: state -> ReadFunction c (EditReader edita) (EditReader editb),
        editUpdate :: edita -> state -> Readable c (EditReader edita) (state,[editb]) -- updates happen after the change, and reads will reflect the new state
    };

    type PureEditFunction = EditFunction Monad;
    type IOEditFunction = EditFunction MonadIO;

    type ObjectFunction = IOEditFunction ();

    editToObjectFunction :: forall c edita editb. EditFunction c ((),()) edita editb -> EditFunction c () edita editb;
    editToObjectFunction (MkEditFunction _ g u) = let
    {
        g' :: () -> ReadFunction c (EditReader edita) (EditReader editb);
        g' () = g ((),());
        u' ea () = do
        {
            (((),()),ebs) <- u ea ((),());
            return ((),ebs);
        };
    } in MkEditFunction () g' u';

    pureToEditFunction :: PureEditFunction state edita editb -> EditFunction c state edita editb;
    pureToEditFunction (MkEditFunction i g u) = MkEditFunction i (\s -> pureToReadFunction $ g s) (\ea s -> pureToReadable $ u ea s);

    editUpdates :: EditFunction c state edita editb -> [edita] -> state -> Readable c (EditReader edita) (state, [editb]);
    editUpdates _ [] st = return (st,[]);
    editUpdates fef (e:ee) oldstate = do
    {
        (midstate,eb1) <- editUpdate fef e oldstate;
        (newstate,eb2) <- editUpdates fef ee midstate;
        return (newstate,eb1 ++ eb2);
    };

    mapEditFunction :: (EditReader editb1 ~ EditReader editb2) =>
     (editb1 -> editb2) -> PureEditFunction state edita editb1 -> PureEditFunction state edita editb2;
    mapEditFunction b12 fef = MkEditFunction
    {
        editInitial = editInitial fef,
        editGet = editGet fef,
        editUpdate = \edita oldstate -> do
        {
            (newstate,meditb1) <- editUpdate fef edita oldstate;
            return (newstate,fmap b12 meditb1);
        }
    };

    comapEditFunction :: (EditReader edita1 ~ EditReader edita2) =>
     (edita2 -> edita1) -> PureEditFunction state edita1 editb -> PureEditFunction state edita2 editb;
    comapEditFunction a21 fef = MkEditFunction
    {
        editInitial = editInitial fef,
        editGet = editGet fef,
        editUpdate = \edita2 -> editUpdate fef (a21 edita2)
    };

    constEditFunction :: forall c edita editb. Reader (EditReader editb) => EditSubject editb -> EditFunction c () edita editb;
    constEditFunction b = let
    {
        editInitial = ();
        editGet :: () -> ReadFunction c (EditReader edita) (EditReader editb);
        editGet () = readFromM $ pure b;
        editUpdate _ () = pure $ pure [];
    } in MkEditFunction{..};

    class StateCategory ff where
    {
        identityState :: forall a. Edit a => ff () a a;
        composeState :: forall a b c s1 s2. (Edit a,Edit b,Edit c) => ff s2 b c -> ff s1 a b -> ff (s1,s2) a c;
    };

    data CloseState ff a b = forall state. MkCloseState (ff state a b);

    instance StateCategory ff => ConstrainedCategory (CloseState ff) where
    {
        type CategoryConstraint (CloseState ff) t = Edit t;
        cid = MkCloseState identityState;
        (MkCloseState bc) <.> (MkCloseState ab) = MkCloseState $ composeState bc ab;
    };

    instance ReadableConstraint c => ConstrainedCategory (EditFunction c ()) where
    {
        type CategoryConstraint (EditFunction c ()) t = Edit t;
        cid = let
        {
            editInitial = ();
            editGet _ = readable;
            editUpdate edit _ = return ((),[edit]);
        } in MkEditFunction{..};
        fef2 <.> fef1 = MkEditFunction
        {
            editInitial = (),
            editGet = \() -> composeReadFunction (editGet fef2 ()) (editGet fef1 ()),
            editUpdate = \editA () -> do
            {
                ((),editBs) <- editUpdate fef1 editA ();
                ((),editCs) <- mapGenReadable (editGet fef1 ()) $ editUpdates fef2 editBs ();
                return ((),editCs);
            }
        };
    };

    instance ReadableConstraint c => StateCategory (EditFunction c) where
    {
        identityState = cid;

        composeState fef2 fef1 = MkEditFunction
        {
            editInitial = (editInitial fef1,editInitial fef2),
            editGet = \(s1,s2) -> composeReadFunction (editGet fef2 s2) (editGet fef1 s1),
            editUpdate = \editA (oldstate1,oldstate2) -> do
            {
                (newstate1,editBs) <- editUpdate fef1 editA oldstate1;
                (newstate2,editCs) <- mapGenReadable (editGet fef1 oldstate1) $ editUpdates fef2 editBs oldstate2;
                return ((newstate1,newstate2),editCs);
            }
        };
    };

    funcEditFunction :: forall c edita editb. (ReadableConstraint c,Edit edita,FullReader c (EditReader edita),FullEdit c editb) =>
        (EditSubject edita -> EditSubject editb) -> EditFunction c () edita editb;
    funcEditFunction ab = let
    {
        editInitial :: ();
        editInitial = ();

        editGet :: () -> ReadFunction c (EditReader edita) (EditReader editb);
        editGet () = simpleReadFunction ab;

        editUpdate :: edita -> () -> Readable c (EditReader edita) ((),[editb]);
        editUpdate edita () = do
        {
            newa <- mapReadable (applyEdit edita) fromReader;
            editbs <- case selfReadable @c @(EditReader edita) of
            {
                MkConstraintWitness -> getReplaceEditsM @c $ ab newa;
            };
            return $ ((),editbs);
        };
    } in MkEditFunction{..};

    convertEditFunction :: forall c edita editb. (ReadableConstraint c,EditSubject edita ~ EditSubject editb,Edit edita,FullReader c (EditReader edita),FullEdit c editb) =>
        EditFunction c () edita editb;
    convertEditFunction = funcEditFunction id;
}
