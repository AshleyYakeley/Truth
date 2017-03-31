module Truth.Edit.FloatingEditFunction  where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;
    import Truth.Edit.Edit;
    import Truth.Edit.WholeEdit;
    import Truth.Edit.Either;
    import Truth.Edit.MaybeReader;
    import Truth.Edit.JustEdit;
    import Truth.Edit.JustWholeEdit;
    import Truth.Edit.EditFunction;


    data FloatingEditFunction state edita editb = MkFloatingEditFunction
    {
        floatingEditInitial :: state,
        floatingEditGet :: state -> ReadFunction (EditReader edita) (EditReader editb),
        floatingEditUpdate :: edita -> state -> (state,[editb])
    };

    floatingEditUpdates :: FloatingEditFunction state edita editb -> [edita] -> state -> (state, [editb]);
    floatingEditUpdates _ [] state = (state,[]);
    floatingEditUpdates fef (e:ee) oldstate = let
    {
        (midstate,eb1) = floatingEditUpdate fef e oldstate;
        (newstate,eb2) = floatingEditUpdates fef ee midstate;
    } in (newstate,eb1 ++ eb2);

    fixedFloatingEditFunction :: forall edita editb. EditFunction edita editb -> FloatingEditFunction () edita editb;
    fixedFloatingEditFunction MkEditFunction{..} = let
    {
        floatingEditInitial = ();
        floatingEditGet :: () -> ReadFunction (EditReader edita) (EditReader editb);
        floatingEditGet _ = editGet;
        floatingEditUpdate edit _ = ((),editUpdate edit);
    } in MkFloatingEditFunction{..};

    mapFloatingEditFunction :: (EditReader editb1 ~ EditReader editb2) =>
     (editb1 -> editb2) -> FloatingEditFunction state edita editb1 -> FloatingEditFunction state edita editb2;
    mapFloatingEditFunction b12 fef = MkFloatingEditFunction
    {
        floatingEditInitial = floatingEditInitial fef,
        floatingEditGet = floatingEditGet fef,
        floatingEditUpdate = \edita oldstate -> let
        {
            (newstate,meditb1) = floatingEditUpdate fef edita oldstate;
        } in (newstate,fmap b12 meditb1)
    };

    comapFloatingEditFunction :: (EditReader edita1 ~ EditReader edita2) =>
     (edita2 -> edita1) -> FloatingEditFunction state edita1 editb -> FloatingEditFunction state edita2 editb;
    comapFloatingEditFunction a21 fef = MkFloatingEditFunction
    {
        floatingEditInitial = floatingEditInitial fef,
        floatingEditGet = floatingEditGet fef,
        floatingEditUpdate = \edita2 -> floatingEditUpdate fef (a21 edita2)
    };

    eitherWholeFloatingEdit :: (Reader (EditReader edita), FullReader (EditReader editb)) =>
     FloatingEditFunction state edita editb ->
     FloatingEditFunction state (EitherWholeEdit edita) (EitherWholeEdit editb);
    eitherWholeFloatingEdit fef = MkFloatingEditFunction
    {
        floatingEditInitial = floatingEditInitial fef,
        floatingEditGet = floatingEditGet fef,
        floatingEditUpdate = \pedita oldstate -> case pedita of
        {
            LeftEdit (MkWholeEdit a) -> let
            {
                b = fromReadFunction (floatingEditGet fef oldstate) a
            } in (oldstate,return $ LeftEdit $ MkWholeEdit b); -- state unchanged, kind of dubious
            RightEdit edita -> let
            {
                (newstate,meditb) = floatingEditUpdate fef edita oldstate;
            } in (newstate,fmap RightEdit meditb);
        }
    };

    justFloatingEdit :: forall f state edita editb. (FunctorOne f,Edit edita,Edit editb) =>
     FloatingEditFunction state edita editb -> FloatingEditFunction state (JustEdit f edita) (JustEdit f editb);
    justFloatingEdit fef = MkFloatingEditFunction
    {
        floatingEditInitial = floatingEditInitial fef,
        floatingEditGet = \state -> liftJustReadFunction (floatingEditGet fef state),
        floatingEditUpdate = \(MkJustEdit edita) oldstate -> let
        {
            (newstate,meditb) = floatingEditUpdate fef edita oldstate;
        } in (newstate,fmap MkJustEdit meditb)
    };

    justWholeFloatingEdit :: forall f state edita editb. (FunctorOne f,Edit edita,Edit editb,FullReader (EditReader editb)) =>
     FloatingEditFunction state edita editb -> FloatingEditFunction state (JustWholeEdit f edita) (JustWholeEdit f editb);
    justWholeFloatingEdit lens = eitherWholeFloatingEdit (justFloatingEdit lens);

    class FloatingMap ff where
    {
        identityFloating :: ff () a a;
        composeFloating :: ff s2 b c -> ff s1 a b -> ff (s1,s2) a c;
    };

    data CloseFloat ff a b = forall state. MkCloseFloat (ff state a b);

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
            floatingEditUpdate = \ea (s1,s2) -> let
            {
                (s1',leb) = floatingEditUpdate fef1 ea s1;
                addeditb eb (oldstate,ecs) = let
                {
                    (newstate,newecs) = floatingEditUpdate fef2 eb oldstate;
                } in (newstate,ecs ++ newecs);
                (s2',lec) = foldr addeditb (s2,[]) leb;
            }
            in ((s1',s2'),lec)
        };
    };
}
