module Truth.Edit.FloatingEditFunction  where
{
    import Truth.Edit.EditFunction;
    import Truth.Edit.JustWholeEdit;
    import Truth.Edit.JustEdit;
    import Truth.Edit.Either;
    import Truth.Edit.WholeEdit;
    import Truth.Edit.Edit;
    import Truth.Edit.ReadFunction;
    import Truth.Edit.Read;
    import Truth.Edit.Import;

    data FloatingEditFunction state edita editb = MkFloatingEditFunction
    {
        floatingEditInitial :: state,
        floatingEditGet :: state -> ReadFunction (EditReader edita) (EditReader editb),
        floatingEditUpdate :: edita -> state -> Readable (EditReader edita) (state,Maybe editb)
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

    mapFloatingEditFunction :: (EditReader editb1 ~ EditReader editb2) =>
     (editb1 -> editb2) -> FloatingEditFunction state edita editb1 -> FloatingEditFunction state edita editb2;
    mapFloatingEditFunction b12 fef = MkFloatingEditFunction
    {
        floatingEditInitial = floatingEditInitial fef,
        floatingEditGet = floatingEditGet fef,
        floatingEditUpdate = \edita oldstate -> fmap (\(newstate,meditb) -> (newstate,fmap b12 meditb)) (floatingEditUpdate fef edita oldstate)
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
            }
            in do
            {
                return (oldstate,Just (LeftEdit (MkWholeEdit b)));
            };
            RightEdit edita -> do
            {
                (newstate,meditb) <- floatingEditUpdate fef edita oldstate;
                return (newstate,fmap RightEdit meditb);
            };
        }
    };

    justFloatingEdit :: forall f state edita editb. (FunctorOne f,Edit edita,Edit editb) =>
     FloatingEditFunction state edita editb -> FloatingEditFunction state (JustEdit f edita) (JustEdit f editb);
    justFloatingEdit fef = MkFloatingEditFunction
    {
        floatingEditInitial = floatingEditInitial fef,
        floatingEditGet = \state -> liftJustReadFunction (floatingEditGet fef state),
        floatingEditUpdate = \(MkJustEdit edita) state -> do
        {
            fsmeb <- liftJustReadable (floatingEditUpdate fef edita state);
            return (case getMaybeOne fsmeb of
            {
                Just (newstate,meditb) -> (newstate,fmap MkJustEdit meditb);
                Nothing -> (state,Nothing);
            });
        }
    };

    justWholeFloatingEdit :: forall f state edita editb. (FunctorOne f,Edit edita,Edit editb,FullReader (EditReader editb)) =>
     FloatingEditFunction state edita editb -> FloatingEditFunction state (JustWholeEdit f edita) (JustWholeEdit f editb);
    justWholeFloatingEdit lens = eitherWholeFloatingEdit (justFloatingEdit lens);

    class FloatingMap ff where
    {
        identityFloating :: ff () a a;
        composeFloating :: ff s2 b c -> ff s1 a b -> ff (s1,s2) a c;
    };

    instance FloatingMap FloatingEditFunction where
    {
        identityFloating = fixedFloatingEditFunction id;

        composeFloating fef2 fef1 = MkFloatingEditFunction
        {
            floatingEditInitial = (floatingEditInitial fef1,floatingEditInitial fef2),
            floatingEditGet = \(s1,s2) -> composeReadFunction (floatingEditGet fef2 s2) (floatingEditGet fef1 s1),
            floatingEditUpdate = \ea (s1,s2) -> do
            {
                (s1',meb) <- floatingEditUpdate fef1 ea s1;
                case meb of
                {
                    Just eb -> do
                    {
                        (s2',mec) <- mapReadable (floatingEditGet fef1 s1') (floatingEditUpdate fef2 eb s2);
                        return ((s1',s2'),mec);
                    };
                    Nothing -> return ((s1',s2),Nothing);
                };
            }
        };
    };
}
