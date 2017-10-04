module Truth.Core.Edit.EditFunction  where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit.Edit;
    import Truth.Core.Edit.FullEdit;


    data EditFunction state edita editb = MkEditFunction
    {
        editAccess :: IOStateAccess state,
        editGet :: state -> ReadFunction (EditReader edita) (EditReader editb),
        editUpdate :: edita -> state -> Readable (EditReader edita) (state,[editb]) -- updates happen after the change, and reads will reflect the new state
    };

    type ObjectFunction = EditFunction ();

    editToObjectFunction :: forall edita editb. EditFunction ((),()) edita editb -> EditFunction () edita editb;
    editToObjectFunction (MkEditFunction _ g u) = let
    {
        g' :: () -> ReadFunction (EditReader edita) (EditReader editb);
        g' () = g ((),());
        u' ea () = do
        {
            (((),()),ebs) <- u ea ((),());
            return ((),ebs);
        };
    } in MkEditFunction unitStateAccess g' u';

    editUpdates :: EditFunction state edita editb -> [edita] -> state -> Readable (EditReader edita) (state, [editb]);
    editUpdates _ [] st = return (st,[]);
    editUpdates fef (e:ee) oldstate = do
    {
        (midstate,eb1) <- editUpdate fef e oldstate;
        (newstate,eb2) <- editUpdates fef ee midstate;
        return (newstate,eb1 ++ eb2);
    };

    mapEditFunction :: (EditReader editb1 ~ EditReader editb2) =>
     (editb1 -> editb2) -> EditFunction state edita editb1 -> EditFunction state edita editb2;
    mapEditFunction b12 fef = MkEditFunction
    {
        editAccess = editAccess fef,
        editGet = editGet fef,
        editUpdate = \edita oldstate -> do
        {
            (newstate,meditb1) <- editUpdate fef edita oldstate;
            return (newstate,fmap b12 meditb1);
        }
    };

    comapEditFunction :: (EditReader edita1 ~ EditReader edita2) =>
     (edita2 -> edita1) -> EditFunction state edita1 editb -> EditFunction state edita2 editb;
    comapEditFunction a21 fef = MkEditFunction
    {
        editAccess = editAccess fef,
        editGet = editGet fef,
        editUpdate = \edita2 -> editUpdate fef (a21 edita2)
    };

    constEditFunction :: forall edita editb. SubjectReader (EditReader editb) => EditSubject editb -> EditFunction () edita editb;
    constEditFunction b = let
    {
        editAccess :: IOStateAccess ();
        editAccess = unitStateAccess;
        editGet :: () -> ReadFunction (EditReader edita) (EditReader editb);
        editGet () = readFromSubjectM $ pure b;
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

    instance Category (EditFunction ()) where
    {
        id = let
        {
            editAccess :: IOStateAccess ();
            editAccess = unitStateAccess;
            editGet _ = readable;
            editUpdate edit _ = return ((),[edit]);
        } in MkEditFunction{..};
        fef2 . fef1 = MkEditFunction
        {
            editAccess = unitStateAccess,
            editGet = \() -> composeReadFunction (editGet fef2 ()) (editGet fef1 ()),
            editUpdate = \editA () -> do
            {
                ((),editBs) <- editUpdate fef1 editA ();
                ((),editCs) <- mapGenReadable (editGet fef1 ()) $ editUpdates fef2 editBs ();
                return ((),editCs);
            }
        };
    };

    instance ConstrainedCategory (EditFunction ()) where
    {
        type CategoryConstraint (EditFunction ()) t = ();
        cid = id;
        (<.>) = (.);
    };

    instance StateCategory EditFunction where
    {
        identityState = cid;

        composeState fef2 fef1 = MkEditFunction
        {
            editAccess = pairStateAccess (editAccess fef1) (editAccess fef2),
            editGet = \(s1,s2) -> composeReadFunction (editGet fef2 s2) (editGet fef1 s1),
            editUpdate = \editA (oldstate1,oldstate2) -> do
            {
                (newstate1,editBs) <- editUpdate fef1 editA oldstate1;
                (newstate2,editCs) <- mapGenReadable (editGet fef1 oldstate1) $ editUpdates fef2 editBs oldstate2;
                return ((newstate1,newstate2),editCs);
            }
        };
    };

    funcEditFunction :: forall edita editb. (Edit edita,FullSubjectReader (EditReader edita),FullEdit editb) =>
        (EditSubject edita -> EditSubject editb) -> EditFunction () edita editb;
    funcEditFunction ab = let
    {
        editAccess :: IOStateAccess ();
        editAccess = unitStateAccess;

        editGet :: () -> ReadFunction (EditReader edita) (EditReader editb);
        editGet () = simpleReadFunction ab;

        editUpdate :: edita -> () -> Readable (EditReader edita) ((),[editb]);
        editUpdate edita () = do
        {
            newa <- mapReadable (applyEdit edita) subjectFromReader;
            editbs <- getReplaceEditsM $ ab newa;
            return $ ((),editbs);
        };
    } in MkEditFunction{..};

    convertEditFunction :: forall edita editb. (EditSubject edita ~ EditSubject editb,Edit edita,FullSubjectReader (EditReader edita),FullEdit editb) =>
        EditFunction () edita editb;
    convertEditFunction = funcEditFunction id;
}
