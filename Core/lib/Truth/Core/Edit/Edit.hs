module Truth.Core.Edit.Edit where
{
    import Truth.Core.Import;
    import Truth.Core.Read;


    class ({- Edit edit -}) => Floating edit (t :: *) where
    {
        floatingUpdate :: edit -> t -> t;
        floatingUpdate _ = id;
    };

    instance Floating edit t => Floating [edit] t where
    {
        floatingUpdate [] = id;
        floatingUpdate (e:ee) = floatingUpdate ee . floatingUpdate e;
    };

    instance HasTypeInfo Floating where
    {
        typeWitness = $(generateWitness [t|Floating|]);
        typeName _ = "Floating";
    };

    class (Reader (EditReader edit),Floating edit edit) => Edit (edit :: *) where
    {
        type EditReader edit :: * -> *;
        applyEdit :: edit -> ReadFunction (EditReader edit) (EditReader edit);
        invertEdit :: edit -> IOReadable (EditReader edit) [edit];
    };
    $(generateFamilyProxy "EditReader");
    type EditSubject edit = ReaderSubject (EditReader edit);

    instance HasTypeInfo Edit where
    {
        typeWitness = $(generateWitness [t|Edit|]);
        typeName _ = "Edit";
    };
{-
    applyAndInvertEdit :: (Edit edit) => edit -> (ReadFunction (EditReader edit) (EditReader edit),Readable (EditReader edit) [edit]);
    applyAndInvertEdit edit = (applyEdit edit,invertEdit edit);
-}
    applyEdits :: (Edit edit) => [edit] -> ReadFunction (EditReader edit) (EditReader edit);
    applyEdits [] = readable;
    applyEdits (e:es) = composeReadFunction (applyEdits es) (applyEdit e);

    -- edits always applied in the given order, so list returned will be reversed relative to list given.
    invertEdits :: (Edit edit) => [edit] -> IOReadable (EditReader edit) [edit];
    invertEdits [] = return [];
    invertEdits (e:ee) = do
    {
        uu <- mapReadable (applyEdit e) $ invertEdits ee;
        u <- invertEdit e;
        return $ u ++ uu;
    };
{-
    commutableEdits :: (Edit edit, Eq (EditSubject edit)) => edit -> edit -> EditSubject edit -> Maybe (EditSubject edit);
    commutableEdits e1 e2 a = let
    {
        cf12 = (applyEdit (updateEdit e2 e1)) . (applyEdit e2);
        cf21 = (applyEdit (updateEdit e1 e2)) . (applyEdit e1);
        a12 = applyConstFunction cf12 a;
        a21 = applyConstFunction cf21 a;
    } in if a12 == a21 then Just a12 else Nothing;
-}

    class (Edit edit,GenFullReader c (EditReader edit)) => GenFullEdit c edit where
    {
        genReplaceEdit :: GenWriterReadable c edit (EditReader edit) ();

        default genReplaceEdit :: FullEdit edit => IOWriterReadable edit (EditReader edit) ();
        genReplaceEdit = writerReadableToGen genReplaceEdit;
    };

    type IOFullEdit = GenFullEdit MonadIO;
    type FullEdit = GenFullEdit Monad;

    ioReplaceEdit :: IOFullEdit edit => IOWriterReadable edit (EditReader edit) ();
    ioReplaceEdit = genReplaceEdit;

    replaceEdit :: FullEdit edit => WriterReadable edit (EditReader edit) ();
    replaceEdit = genReplaceEdit;

    getReplaceEditsM :: forall c m edit. (ReadableConstraint c,GenFullEdit c edit,Monad m,c m) => EditSubject edit -> m [edit];
    getReplaceEditsM = fromGenReadable (writerToReadable genReplaceEdit :: GenReadable c (EditReader edit) [edit]);

    getReplaceEdits :: forall edit. FullEdit edit => EditSubject edit -> [edit];
    getReplaceEdits = fromReadable (writerToReadable replaceEdit :: Readable (EditReader edit) [edit]);

    instance HasTypeInfo GenFullEdit where
    {
        typeWitness = $(generateWitness [t|GenFullEdit|]);
        typeName _ = "GenFullEdit";
    };
}
