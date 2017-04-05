module Truth.Core.Types.ComonadLift where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Comonad;


    comonadLiftEditFunction :: forall w edita editb. EditFunction edita editb -> EditFunction (ComonadEdit w edita) (ComonadEdit w editb);
    comonadLiftEditFunction (MkEditFunction eg eu) = let
    {
        editGet :: ReadFunction (ComonadReader w (EditReader edita)) (ComonadReader w (EditReader editb));
        editGet = comonadLiftReadFunction eg;

        editUpdate (MkComonadEdit edit) = fmap MkComonadEdit $ eu edit;
    } in MkEditFunction{..};

    comonadLiftEditLens :: Functor m => EditLens' m edita editb -> EditLens' m (ComonadEdit w edita) (ComonadEdit w editb);
    comonadLiftEditLens (MkEditLens ef epe) = let
    {
        editLensFunction = comonadLiftEditFunction ef;
        editLensPutEdit (MkComonadEdit edit) = fmap (fmap MkComonadEdit) $ mapReadable comonadReadFunction $ epe edit;
    } in MkEditLens{..};
}
