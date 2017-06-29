module Truth.Core.Edit.FloatingEditFunction  where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit.Edit;
    import Truth.Core.Edit.EditFunction;


    data FloatingEditFunction state edita editb = MkFloatingEditFunction
    {
        floatingEditInitial :: state,
        floatingEditGet :: state -> ReadFunction (EditReader edita) (EditReader editb),
        floatingEditUpdate :: edita -> state -> Readable (EditReader edita) (state,[editb])
    };

    floatingEditUpdates :: FloatingEditFunction state edita editb -> [edita] -> state -> Readable (EditReader edita) (state, [editb]);
    floatingEditUpdates _ [] st = return (st,[]);
    floatingEditUpdates fef (e:ee) oldstate = do
    {
        (midstate,eb1) <- floatingEditUpdate fef e oldstate;
        (newstate,eb2) <- floatingEditUpdates fef ee midstate;
        return (newstate,eb1 ++ eb2);
    };

    fixedFloatingEditFunction :: forall edita editb. EditFunction edita editb -> FloatingEditFunction () edita editb;
    fixedFloatingEditFunction MkEditFunction{..} = let
    {
        floatingEditInitial = ();
        floatingEditGet :: () -> ReadFunction (EditReader edita) (EditReader editb);
        floatingEditGet _ = editGet;
        floatingEditUpdate edit _ = do
        {
            editBs <- editUpdate edit;
            return ((),editBs);
        };
    } in MkFloatingEditFunction{..};

    mapFloatingEditFunction :: (EditReader editb1 ~ EditReader editb2) =>
     (editb1 -> editb2) -> FloatingEditFunction state edita editb1 -> FloatingEditFunction state edita editb2;
    mapFloatingEditFunction b12 fef = MkFloatingEditFunction
    {
        floatingEditInitial = floatingEditInitial fef,
        floatingEditGet = floatingEditGet fef,
        floatingEditUpdate = \edita oldstate -> do
        {
            (newstate,meditb1) <- floatingEditUpdate fef edita oldstate;
            return (newstate,fmap b12 meditb1);
        }
    };

    comapFloatingEditFunction :: (EditReader edita1 ~ EditReader edita2) =>
     (edita2 -> edita1) -> FloatingEditFunction state edita1 editb -> FloatingEditFunction state edita2 editb;
    comapFloatingEditFunction a21 fef = MkFloatingEditFunction
    {
        floatingEditInitial = floatingEditInitial fef,
        floatingEditGet = floatingEditGet fef,
        floatingEditUpdate = \edita2 -> floatingEditUpdate fef (a21 edita2)
    };

    class FloatingMap ff where
    {
        identityFloating :: ff () a a;
        composeFloating :: ff s2 b c -> ff s1 a b -> ff (s1,s2) a c;
    };

    data CloseFloat ff a b = forall state. Eq state => MkCloseFloat (ff state a b);

    instance FloatingMap ff => Category (CloseFloat ff) where
    {
        id = MkCloseFloat identityFloating;
        (MkCloseFloat bc) . (MkCloseFloat ab) = MkCloseFloat $ composeFloating bc ab;
    };

    instance FloatingMap FloatingEditFunction where
    {
        identityFloating = fixedFloatingEditFunction id;

        composeFloating fef2 fef1 = MkFloatingEditFunction
        {
            floatingEditInitial = (floatingEditInitial fef1,floatingEditInitial fef2),
            floatingEditGet = \(s1,s2) -> composeReadFunction (floatingEditGet fef2 s2) (floatingEditGet fef1 s1),
            floatingEditUpdate = \editA (oldstate1,oldstate2) -> do
            {
                (newstate1,editBs) <- floatingEditUpdate fef1 editA oldstate1;
                (newstate2,editCs) <- mapReadable (floatingEditGet fef1 oldstate1) $ floatingEditUpdates fef2 editBs oldstate2;
                return ((newstate1,newstate2),editCs);
            }
        };
    };
}
