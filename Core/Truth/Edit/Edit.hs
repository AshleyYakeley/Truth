module Truth.Edit.Edit where
{
    import Truth.Edit.Import;

    class Edit edit where
    {
        type Subject edit;
        applyEdit :: edit -> ConstFunction (Subject edit) (Subject edit);
        invertEdit :: edit -> Subject edit -> Maybe edit;    -- "Nothing" means no change
        updateEdit :: edit -> edit -> edit;
        updateEdit _ = id;
    };

    -- subjectRep :: HasInfo (Type_T (Subject edit)) => Info (Type_T edit) -> Info (Type_T (Subject edit));
    -- subjectRep _ = info;

    data Edit_Inst edit where
    {
        Edit_Inst :: forall edit. (Edit edit) => Info (Type_T (Subject edit)) -> Edit_Inst (Type_T edit);
    };
    $(factInstances [t|Edit_Inst|]);

    applyAndInvertEdit :: (Edit edit) => edit -> (ConstFunction (Subject edit) (Subject edit),(Subject edit) -> Maybe edit);
    applyAndInvertEdit edit = (applyEdit edit,invertEdit edit);

    applyEdits :: (Edit edit) => [edit] -> ConstFunction (Subject edit) (Subject edit);
    applyEdits [] = id;
    applyEdits (e:es) = (applyEdits es) . (applyEdit e);

    commutableEdits :: (Edit edit, Eq (Subject edit)) => edit -> edit -> Subject edit -> Maybe (Subject edit);
    commutableEdits e1 e2 a = let
    {
        cf12 = (applyEdit (updateEdit e2 e1)) . (applyEdit e2);
        cf21 = (applyEdit (updateEdit e1 e2)) . (applyEdit e1);
        a12 = applyConstFunction cf12 a;
        a21 = applyConstFunction cf21 a;
    } in if a12 == a21 then Just a12 else Nothing;

    class (Edit edit) => FullEdit edit where
    {
        replaceEdit :: Subject edit -> edit;
    };

    data FullEdit_Inst edit where
    {
        FullEdit_Inst :: forall edit. (FullEdit edit) => FullEdit_Inst (Type_T edit);
    };
    $(factInstances [t|FullEdit_Inst|]);
}
