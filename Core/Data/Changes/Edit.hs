module Data.Changes.Edit where
{
    import Data.TypeKT;
    import Data.OpenWitness;
    import Data.ConstFunction;
    import Control.Category;
    import Prelude hiding (id,(.));

    class Edit edit where
    {
        type Subject edit;
        applyEdit :: edit -> ConstFunction (Subject edit) (Subject edit);
        invertEdit :: edit -> Subject edit -> Maybe edit;    -- "Nothing" means no change
    };

    subjectRepT :: HasInfoT (Subject edit) => InfoT edit -> InfoT (Subject edit);
    subjectRepT _ = infoT;

    data EditInst edit where
    {
        MkEditInst :: forall edit. (Edit edit) => InfoT (Subject edit) -> EditInst edit;
    };

    instance PropertyT EditInst where
    {
        matchPropertyT = matchPropertyT_Fact;
    };

    instance FactT EditInst where
    {
        witFactT = unsafeIOWitnessFromString "Data.Changes.Edit.EditInst";
    };

    applyAndInvertEdit :: (Edit edit) => edit -> (ConstFunction (Subject edit) (Subject edit),(Subject edit) -> Maybe edit);
    applyAndInvertEdit edit = (applyEdit edit,invertEdit edit);

    applyEdits :: (Edit edit) => [edit] -> ConstFunction (Subject edit) (Subject edit);
    applyEdits [] = id;
    applyEdits (e:es) = (applyEdits es) . (applyEdit e);

    commutableEdits :: (Edit edit, Eq (Subject edit)) => edit -> edit -> Subject edit -> Maybe (Subject edit);
    commutableEdits e1 e2 a = let
    {
        cf1 = applyEdit e1;
        cf2 = applyEdit e2;
        cf12 = cf1 . cf2;
        cf21 = cf2 . cf1;
        a12 = applyConstFunction cf12 a;
        a21 = applyConstFunction cf21 a;
    } in if a12 == a21 then Just a12 else Nothing;

    class (Edit edit) => FullEdit edit where
    {
        replaceEdit :: Subject edit -> edit;
    };

    data FullEditInst edit where
    {
        MkFullEditInst :: forall edit. (FullEdit edit) => FullEditInst edit;
    };

    instance PropertyT FullEditInst where
    {
        matchPropertyT = matchPropertyT_Fact;
    };

    instance FactT FullEditInst where
    {
        witFactT = unsafeIOWitnessFromString "Data.Changes.Edit.FullEditInst";
    };
}
