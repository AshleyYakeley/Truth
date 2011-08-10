module Truth.Edit.Edit where
{
    import Truth.Edit.ReadFunction;
    import Truth.Edit.Read;
    import Truth.Edit.Import;

    class (Reader (EditReader edit)) => Edit edit where
    {
        type EditReader edit :: * -> *;
        applyEdit :: edit -> ReadFunction (EditReader edit) (EditReader edit);
        invertEdit :: edit -> Readable (EditReader edit) (Maybe edit);    -- "Nothing" means no change
        updateEdit :: edit -> edit -> edit;
        updateEdit _ = id;
    };
    type EditSubject edit = Subject (EditReader edit);

    -- subjectRep :: HasInfo (Type_T (Subject edit)) => Info (Type_T edit) -> Info (Type_T (Subject edit));
    -- subjectRep _ = info;

    data Edit_Inst edit where
    {
        Edit_Inst :: forall edit. (Edit edit) => Info (Type_KTT (EditReader edit)) -> Edit_Inst (Type_T edit);
    };
    $(factInstances [t|Edit_Inst|]);

    applyAndInvertEdit :: (Edit edit) => edit -> (ReadFunction (EditReader edit) (EditReader edit),Readable (EditReader edit) (Maybe edit));
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

    data FullEdit_Inst edit where
    {
        FullEdit_Inst :: forall edit. (FullEdit edit) => FullEdit_Inst (Type_T edit);
    };
    $(factInstances [t|FullEdit_Inst|]);
}
