module Truth.Core.Types.Savable where
{
    import Truth.Core.Import;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types.Whole;


    data SaveBuffer a = MkSaveBuffer
    {
        saveBuffer :: a,
        saveBufferChanged :: Bool
    };

    data SaveAction a = SASave | SARevert | SAChange a;

    saveBufferLens :: forall a. (EditReader (SaveAction a) ~ WholeReader a) =>
        FloatingEditLens (SaveBuffer a) (WholeEdit (WholeReader a)) (SaveAction a);
    saveBufferLens = let
    {
        floatingEditInitial :: SaveBuffer a;
        floatingEditInitial = MkSaveBuffer undefined False;

        floatingEditGet :: SaveBuffer a -> ReadFunction (WholeReader a) (WholeReader a);
        floatingEditGet (MkSaveBuffer buffer _) ReadWhole = return buffer;

        floatingEditUpdate :: WholeEdit (WholeReader a) -> SaveBuffer a -> Readable (WholeReader a) (SaveBuffer a,[SaveAction a]);
        floatingEditUpdate (MkWholeEdit buffer) _ = return (MkSaveBuffer buffer False,[SAChange buffer]);

        floatingEditLensFunction :: FloatingEditFunction (SaveBuffer a) (WholeEdit (WholeReader a)) (SaveAction a);
        floatingEditLensFunction = MkFloatingEditFunction{..};

        floatingEditLensPutEdit :: SaveBuffer a -> SaveAction a -> Readable (WholeReader a) (Maybe (SaveBuffer a,[WholeEdit (WholeReader a)]));
        floatingEditLensPutEdit (MkSaveBuffer _ False) SASave = return Nothing;
        floatingEditLensPutEdit (MkSaveBuffer buffer True) SASave = return $ Just (MkSaveBuffer buffer False,[MkWholeEdit buffer]);
        floatingEditLensPutEdit (MkSaveBuffer _ False) SARevert = return Nothing;
        floatingEditLensPutEdit (MkSaveBuffer _ True) SARevert = do
        {
            buffer <- readable ReadWhole;
            return $ Just (MkSaveBuffer buffer False,[]);
        };
        floatingEditLensPutEdit (MkSaveBuffer _ _) (SAChange buffer) = return $ Just (MkSaveBuffer buffer True,[]);
    } in MkFloatingEditLens{..};
}
