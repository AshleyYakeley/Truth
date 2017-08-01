module Truth.Core.Types.ComonadLift where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Comonad;


    comonadLiftEditFunction :: forall c state w edita editb. (ReadableConstraint c) =>
        GenFloatingEditFunction c state edita editb -> GenFloatingEditFunction c state (ComonadEdit w edita) (ComonadEdit w editb);
    comonadLiftEditFunction (MkFloatingEditFunction floatingEditInitial eg eu) = let
    {
        floatingEditGet :: state -> GenReadFunction c (ComonadReader w (EditReader edita)) (ComonadReader w (EditReader editb));
        floatingEditGet curstate = comonadLiftReadFunction $ eg curstate;

        floatingEditUpdate :: ComonadEdit w edita -> state -> GenReadable c (ComonadReader w (EditReader edita)) (state,[ComonadEdit w editb]);
        floatingEditUpdate (MkComonadEdit edita) oldstate = mapReadable comonadReadFunction $ do
        {
            (newstate,editbs) <- eu edita oldstate;
            return (newstate,fmap MkComonadEdit editbs);
        };
    } in MkFloatingEditFunction{..};

    comonadLiftEditLens :: forall c m state w edita editb. (ReadableConstraint c,Functor m) =>
        GenFloatingEditLens' c m state edita editb -> GenFloatingEditLens' c m state (ComonadEdit w edita) (ComonadEdit w editb);
    comonadLiftEditLens (MkFloatingEditLens ef epe) = let
    {
        floatingEditLensFunction = comonadLiftEditFunction ef;
        floatingEditLensPutEdit :: state -> ComonadEdit w editb -> GenReadable c (ComonadReader w (EditReader edita)) (m (state,[ComonadEdit w edita]));
        floatingEditLensPutEdit oldstate (MkComonadEdit editb) = mapReadable comonadReadFunction $ do
        {
            fr <- epe oldstate editb;
            return $ fmap (\(newstate,editas) -> (newstate,fmap MkComonadEdit editas)) fr;
        };
    } in MkFloatingEditLens{..};
}
