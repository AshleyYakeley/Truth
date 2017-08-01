module Truth.Core.Edit.FloatingEditFunction  where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit.Edit;
    import Truth.Core.Edit.FullEdit;


    data GenFloatingEditFunction c state edita editb = MkFloatingEditFunction
    {
        floatingEditInitial :: state,
        floatingEditGet :: state -> GenReadFunction c (EditReader edita) (EditReader editb),
        floatingEditUpdate :: edita -> state -> GenReadable c (EditReader edita) (state,[editb])
    };

    type FloatingEditFunction = GenFloatingEditFunction Monad;
    type IOFloatingEditFunction = GenFloatingEditFunction MonadIO;

    floatingEditFunctionToGen :: FloatingEditFunction state edita editb -> GenFloatingEditFunction c state edita editb;
    floatingEditFunctionToGen (MkFloatingEditFunction i g u) = MkFloatingEditFunction i (\s -> readFunctionToGen $ g s) (\ea s -> readableToGen $ u ea s);

    floatingEditUpdates :: GenFloatingEditFunction c state edita editb -> [edita] -> state -> GenReadable c (EditReader edita) (state, [editb]);
    floatingEditUpdates _ [] st = return (st,[]);
    floatingEditUpdates fef (e:ee) oldstate = do
    {
        (midstate,eb1) <- floatingEditUpdate fef e oldstate;
        (newstate,eb2) <- floatingEditUpdates fef ee midstate;
        return (newstate,eb1 ++ eb2);
    };
{-
    fixedFloatingEditFunction :: forall c edita editb. GenEditFunction c edita editb -> GenFloatingEditFunction c () edita editb;
    fixedFloatingEditFunction MkFloatingEditFunction{..} = let
    {
        floatingEditInitial = ();
        floatingEditGet :: () -> GenReadFunction c (EditReader edita) (EditReader editb);
        floatingEditGet _ = floatingEditGet _;
        floatingEditUpdate edit _ = do
        {
            editBs <- floatingEditUpdate edit;
            return ((),editBs);
        };
    } in MkFloatingEditFunction{..};
-}
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
        identityFloating :: forall a. Edit a => ff () a a;
        composeFloating :: forall a b c s1 s2. (Edit a,Edit b,Edit c) => ff s2 b c -> ff s1 a b -> ff (s1,s2) a c;
    };

    data CloseFloat ff a b = forall state. Eq state => MkCloseFloat (ff state a b);

    instance FloatingMap ff => ConstrainedCategory (CloseFloat ff) where
    {
        type CategoryConstraint (CloseFloat ff) t = Edit t;
        cid = MkCloseFloat identityFloating;
        (MkCloseFloat bc) <.> (MkCloseFloat ab) = MkCloseFloat $ composeFloating bc ab;
    };

    instance ReadableConstraint c => ConstrainedCategory (GenFloatingEditFunction c ()) where
    {
        type CategoryConstraint (GenFloatingEditFunction c ()) t = Edit t;
        cid = let
        {
            floatingEditInitial = ();
            floatingEditGet _ = readable;
            floatingEditUpdate edit _ = return ((),[edit]);
        } in MkFloatingEditFunction{..};
        fef2 <.> fef1 = MkFloatingEditFunction
        {
            floatingEditInitial = (),
            floatingEditGet = \() -> composeReadFunction (floatingEditGet fef2 ()) (floatingEditGet fef1 ()),
            floatingEditUpdate = \editA () -> do
            {
                ((),editBs) <- floatingEditUpdate fef1 editA ();
                ((),editCs) <- mapGenReadable (floatingEditGet fef1 ()) $ floatingEditUpdates fef2 editBs ();
                return ((),editCs);
            }
        };
    };

    instance ReadableConstraint c => FloatingMap (GenFloatingEditFunction c) where
    {
        identityFloating = cid;

        composeFloating fef2 fef1 = MkFloatingEditFunction
        {
            floatingEditInitial = (floatingEditInitial fef1,floatingEditInitial fef2),
            floatingEditGet = \(s1,s2) -> composeReadFunction (floatingEditGet fef2 s2) (floatingEditGet fef1 s1),
            floatingEditUpdate = \editA (oldstate1,oldstate2) -> do
            {
                (newstate1,editBs) <- floatingEditUpdate fef1 editA oldstate1;
                (newstate2,editCs) <- mapGenReadable (floatingEditGet fef1 oldstate1) $ floatingEditUpdates fef2 editBs oldstate2;
                return ((newstate1,newstate2),editCs);
            }
        };
    };

    convertEditFunction :: forall c edita editb. (ReadableConstraint c,EditSubject edita ~ EditSubject editb,Edit edita,GenFullReader c (EditReader edita),GenFullEdit c editb) =>
        GenFloatingEditFunction c () edita editb;
    convertEditFunction = let
    {
        floatingEditInitial :: ();
        floatingEditInitial = ();

        floatingEditGet :: () -> GenReadFunction c (EditReader edita) (EditReader editb);
        floatingEditGet () = convertReadFunction;

        floatingEditUpdate :: edita -> () -> GenReadable c (EditReader edita) ((),[editb]);
        floatingEditUpdate edita () = do
        {
            newsubject <- mapReadable (applyEdit edita) genFromReader;
            editbs <- case selfReadable @c @(EditReader edita) of
            {
                MkConstraintWitness -> getReplaceEditsM @c newsubject;
            };
            return $ ((),editbs);
        };
    } in MkFloatingEditFunction{..};
}
