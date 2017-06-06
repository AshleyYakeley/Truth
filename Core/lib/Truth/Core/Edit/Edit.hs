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

    instance HasInfo Floating where
    {
        info = mkSimpleInfo $(iowitness[t|Floating|]) [];
    };

    class (Reader (EditReader edit),Floating edit edit) => Edit (edit :: *) where
    {
        type EditReader edit :: * -> *;
        applyEdit :: edit -> ReadFunction (EditReader edit) (EditReader edit);
        invertEdit :: edit -> Readable (EditReader edit) [edit];
    };
    $(typeFamilyProxy "EditReader");
    type EditSubject edit = ReaderSubject (EditReader edit);

    instance HasInfo Edit where
    {
        info = mkSimpleInfo $(iowitness[t|Edit|]) [];
    };
{-
    applyAndInvertEdit :: (Edit edit) => edit -> (ReadFunction (EditReader edit) (EditReader edit),Readable (EditReader edit) [edit]);
    applyAndInvertEdit edit = (applyEdit edit,invertEdit edit);
-}
    applyEdits :: (Edit edit) => [edit] -> ReadFunction (EditReader edit) (EditReader edit);
    applyEdits [] = readable;
    applyEdits (e:es) = composeReadFunction (applyEdits es) (applyEdit e);

    -- edits always applied in the given order, so list returned will be reversed relative to list given.
    invertEdits :: (Edit edit) => [edit] -> Readable (EditReader edit) [edit];
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

    class (Edit edit,FullReader (EditReader edit)) => FullEdit edit where
    {
        replaceEdit :: WriterReadable edit (EditReader edit) ();
    };

    getReplaceEdits :: FullEdit edit => EditSubject edit -> [edit];
    getReplaceEdits = fromReadable (writerToReadable replaceEdit);

    instance HasInfo FullEdit where
    {
        info = mkSimpleInfo $(iowitness[t|FullEdit|]) [];
    };
}
