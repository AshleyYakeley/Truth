module Truth.Edit.FloatingEditFunction  where
{
    import Truth.Edit.EditFunction;
    import Truth.Edit.JustWholeEdit;
    import Truth.Edit.JustEdit;
    import Truth.Edit.WholeEdit;
    import Truth.Edit.Edit;
    import Truth.Edit.Import;

    data FloatingEditFunction state edita editb = MkFloatingEditFunction
    {
        floatingEditInitial :: state,
        floatingEditGet :: state -> Subject edita -> Subject editb,
        floatingEditUpdate :: edita -> state -> ConstFunction (Subject edita) (state,Maybe editb)
    };

    fixedFloatingEditFunction :: EditFunction edita editb -> FloatingEditFunction () edita editb;
    fixedFloatingEditFunction ef = MkFloatingEditFunction
    {
        floatingEditInitial = (),
        floatingEditGet = \_ -> editGet ef,
        floatingEditUpdate = \edit _ -> do
        {
            meb <- editUpdate ef edit;
            return ((),meb);
        }
    };

    mapFloatingEditFunction :: (Subject editb1 ~ Subject editb2) =>
     (editb1 -> editb2) -> FloatingEditFunction state edita editb1 -> FloatingEditFunction state edita editb2;
    mapFloatingEditFunction b12 fef = MkFloatingEditFunction
    {
        floatingEditInitial = floatingEditInitial fef,
        floatingEditGet = floatingEditGet fef,
        floatingEditUpdate = \edita oldstate -> fmap (\(newstate,meditb) -> (newstate,fmap b12 meditb)) (floatingEditUpdate fef edita oldstate)
    };

    comapFloatingEditFunction :: (Subject edita1 ~ Subject edita2) =>
     (edita2 -> edita1) -> FloatingEditFunction state edita1 editb -> FloatingEditFunction state edita2 editb;
    comapFloatingEditFunction a21 fef = MkFloatingEditFunction
    {
        floatingEditInitial = floatingEditInitial fef,
        floatingEditGet = floatingEditGet fef,
        floatingEditUpdate = \edita2 -> floatingEditUpdate fef (a21 edita2)
    };

    eitherWholeFloatingEdit ::
     FloatingEditFunction state edita editb ->
     FloatingEditFunction state (Either (WholeEdit (Subject edita)) edita) (Either (WholeEdit (Subject editb)) editb);
    eitherWholeFloatingEdit fef = MkFloatingEditFunction
    {
        floatingEditInitial = floatingEditInitial fef,
        floatingEditGet = floatingEditGet fef,
        floatingEditUpdate = \pedita oldstate -> case pedita of
        {
            Left (MkWholeEdit a) -> return (oldstate,Just (Left (MkWholeEdit (floatingEditGet fef oldstate a))));
            Right edita -> do
            {
                (newstate,meditb) <- floatingEditUpdate fef edita oldstate;
                return (newstate,fmap Right meditb);
            };
        }
    };

    justFloatingEdit :: forall f state edita editb. (FunctorOne f,Edit edita,Edit editb) =>
     FloatingEditFunction state edita editb -> FloatingEditFunction state (JustEdit f edita) (JustEdit f editb);
    justFloatingEdit fef = MkFloatingEditFunction
    {
        floatingEditInitial = floatingEditInitial fef,
        floatingEditGet = \state -> cfmap (floatingEditGet fef state),
        floatingEditUpdate = \(MkJustEdit edita) state -> do
        {
            msmeb <-  cofmap1CF getMaybeOne (cfmap (floatingEditUpdate fef edita state));
            return (case msmeb of
            {
                Just (newstate,meditb) -> (newstate,fmap MkJustEdit meditb);
                Nothing -> (state,Nothing);
            });
        }
    };

    justWholeFloatingEdit :: forall f state edita editb. (FunctorOne f,Edit edita,Edit editb) =>
     FloatingEditFunction state edita editb -> FloatingEditFunction state (JustWholeEdit f edita) (JustWholeEdit f editb);
    justWholeFloatingEdit lens = eitherWholeFloatingEdit (justFloatingEdit lens);
}
