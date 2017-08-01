module Truth.Core.Types.ComonadLift where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Comonad;


    comonadLiftEditFunction :: forall c state w edita editb. (ReadableConstraint c) =>
        EditFunction c state edita editb -> EditFunction c state (ComonadEdit w edita) (ComonadEdit w editb);
    comonadLiftEditFunction (MkEditFunction editInitial eg eu) = let
    {
        editGet :: state -> ReadFunction c (ComonadReader w (EditReader edita)) (ComonadReader w (EditReader editb));
        editGet curstate = comonadLiftReadFunction $ eg curstate;

        editUpdate :: ComonadEdit w edita -> state -> Readable c (ComonadReader w (EditReader edita)) (state,[ComonadEdit w editb]);
        editUpdate (MkComonadEdit edita) oldstate = mapReadable comonadReadFunction $ do
        {
            (newstate,editbs) <- eu edita oldstate;
            return (newstate,fmap MkComonadEdit editbs);
        };
    } in MkEditFunction{..};

    comonadLiftEditLens :: forall c m state w edita editb. (ReadableConstraint c,Functor m) =>
        EditLens' c m state edita editb -> EditLens' c m state (ComonadEdit w edita) (ComonadEdit w editb);
    comonadLiftEditLens (MkEditLens ef epe) = let
    {
        editLensFunction = comonadLiftEditFunction ef;
        editLensPutEdit :: state -> ComonadEdit w editb -> Readable c (ComonadReader w (EditReader edita)) (m (state,[ComonadEdit w edita]));
        editLensPutEdit oldstate (MkComonadEdit editb) = mapReadable comonadReadFunction $ do
        {
            fr <- epe oldstate editb;
            return $ fmap (\(newstate,editas) -> (newstate,fmap MkComonadEdit editas)) fr;
        };
    } in MkEditLens{..};
}
