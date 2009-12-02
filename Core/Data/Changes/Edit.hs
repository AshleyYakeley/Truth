{-# OPTIONS -fno-warn-orphans #-}
module Data.Changes.Edit where
{
    import Data.TypeKT;
    import Data.OpenWitness;
    import Data.Witness;
    import Data.ConstFunction;
    import Data.Nothing;
    import Control.Category;
    import Prelude hiding (id,(.));

    class Edit edit where
    {
        type Subject edit;
        applyEdit :: edit -> ConstFunction (Subject edit) (Subject edit);
        invertEdit :: edit -> Subject edit -> Maybe edit;    -- "Nothing" means no change
    };

    subjectRepT :: HasTypeT (Subject edit) => TypeT edit -> TypeT (Subject edit);
    subjectRepT _ = typeT;


    data EditInst edit where
    {
        MkEditInst :: forall edit. (Edit edit) => TypeT (Subject edit) -> EditInst edit;
    };

    instance TypeFactT EditInst where
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

    instance TypeFactT FullEditInst where
    {
        witFactT = unsafeIOWitnessFromString "Data.Changes.Edit.FullEditInst";
    };

    newtype NoEdit a = MkNoEdit Nothing;

    instance Edit (NoEdit a) where
    {
        type Subject (NoEdit a) = a;
        applyEdit (MkNoEdit n) = never n;
        invertEdit (MkNoEdit n) = never n;
    };

    instance HasTypeKTT NoEdit where
    {
        typeKTT = MkTypeKTT
            (WitKTT (unsafeIOWitnessFromString "Data.Changes.Edit.NoEdit"))
            (mkTInfoKTT (\ta -> return (MkEditInst ta)));
    };

    instance (Edit ea,Edit eb,Subject ea ~ Subject eb) => Edit (Either ea eb) where
    {
        type Subject (Either ea eb) = Subject ea;

        applyEdit (Left edit) = applyEdit edit;
        applyEdit (Right edit) = applyEdit edit;

        invertEdit (Left edit) s = fmap Left (invertEdit edit s);
        invertEdit (Right edit) s = fmap Right (invertEdit edit s);
    };

    instance (FullEdit ea,Edit eb,Subject ea ~ Subject eb) => FullEdit (Either ea eb) where
    {
        replaceEdit s = Left (replaceEdit s);
    };

    instance HasTypeKTKTT Either where
    {
        typeKTKTT = MkTypeKTKTT
            (WitKTKTT (unsafeIOWitnessFromString "Data.Changes.Edit.Either"))
            (
                (mkTInfoKTKTT_ (witFactT :: IOWitness (SatKTT EditInst)) (\ta tb -> do
                {
                    MkEditInst sa <- typeFactT ta;
                    MkEditInst sb <- typeFactT tb;
                    MkEqualType <- matchWitnessT sa sb;
                    return (MkEditInst sa);
                }) :: InfoKTKTT Either) `mappend`
                (mkTInfoKTKTT_ (witFactT :: IOWitness (SatKTT FullEditInst)) (\ta tb -> do
                {
                    MkEditInst sa <- typeFactT ta;
                    MkFullEditInst <- typeFactT ta;
                    MkEditInst sb <- typeFactT tb;
                    MkEqualType <- matchWitnessT sa sb;
                    return MkFullEditInst;
                }) :: InfoKTKTT Either)
            );
    };
}
