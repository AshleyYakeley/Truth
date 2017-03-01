module Truth.Edit.Edit where
{
    import Truth.Edit.Import;
    import Truth.Edit.Read;

    class ({- Edit (FloatingEdit t) -}) => Floating (t :: *) where
    {
        type FloatingEdit t :: *;

        floatingUpdate :: FloatingEdit t -> t -> t;
        floatingUpdate _ = id;
    };

    class (Reader (EditReader edit),Floating edit,edit ~ FloatingEdit edit) => Edit (edit :: *) where
    {
        type EditReader edit :: * -> *;
        applyEdit :: edit -> ReadFunction (EditReader edit) (EditReader edit);
        invertEdit :: edit -> Readable (EditReader edit) [edit];
    };
    type EditSubject edit = ReaderSubject (EditReader edit);

    -- subjectRep :: HasInfo (Type_T (ReaderSubject edit)) => Info (Type_T edit) -> Info (Type_T (ReaderSubject edit));
    -- subjectRep _ = info;


    data Edit_Inst :: * -> * where
    {
        Edit_Inst :: forall edit. (Edit edit) => Info (EditReader edit) -> Edit_Inst edit;
    };

    instance HasInfo Edit_Inst where
    {
        info = mkSimpleInfo $(iowitness[t|Edit_Inst|]) [];
    };

    applyAndInvertEdit :: (Edit edit) => edit -> (ReadFunction (EditReader edit) (EditReader edit),Readable (EditReader edit) [edit]);
    applyAndInvertEdit edit = (applyEdit edit,invertEdit edit);

    applyEdits :: (Edit edit) => [edit] -> ReadFunction (EditReader edit) (EditReader edit);
    applyEdits [] = readable;
    applyEdits (e:es) = composeReadFunction (applyEdits es) (applyEdit e);
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
        replaceEdit :: EditSubject edit -> edit;
    };

    instance HasInfo FullEdit where
    {
        info = mkSimpleInfo $(iowitness[t|FullEdit|]) [];
    };
}
