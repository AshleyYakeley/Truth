module Data.Changes.Edit where
{
    import Data.Changes.HasTypeRep;
    import Data.Changes.EditRep;
    import Data.TypeKT.WitnessKT;
    import Data.OpenWitness;
    import Data.Witness;
    import Data.ConstFunction;
    import Data.Nothing;
    import Control.Category;
    import Prelude hiding (id,(.));

    class (HasTypeRepT edit,HasTypeRepT (Subject edit)) => Edit edit where
    {
        type Subject edit;
        applyEdit :: edit -> ConstFunction (Subject edit) (Subject edit);
        invertEdit :: edit -> Subject edit -> Maybe edit;    -- "Nothing" means no change

        type EditStructure edit :: * -> *;
        editStructure :: EditStructure edit edit;
        matchEditStructure :: forall t. (Edit t) => EditRepT t -> Maybe (EditStructure edit t);
    };
{-
    data EditInst edit where
    {
        MkEditInst :: forall edit. (Edit edit) => EditInst edit;
    };

    editInstEvidence :: EditInst edit -> EditEvidence edit;
    editInstEvidence ei@MkEditInst = editEvidence ei;
-}
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
{-
    data FullEditInst edit where
    {
        MkFullEditInst :: forall edit. (FullEdit edit) => FullEditInst edit;
    };
-}
    newtype NoEdit a = MkNoEdit Nothing;

    instance HasTypeRepKTT NoEdit where
    {
        typeRepKTT = EditRepKTT (unsafeIOWitnessFromString "Data.Changes.Edit.NoEdit");
    };

    data NoStructure edit where
    {
        MkNoStructure :: forall a. (HasTypeRepT a) => NoStructure (NoEdit a);
    };

    instance (HasTypeRepT a) => Edit (NoEdit a) where
    {
        type Subject (NoEdit a) = a;
        applyEdit (MkNoEdit n) = never n;
        invertEdit (MkNoEdit n) = never n;

        type EditStructure (NoEdit a) = NoStructure;
        editStructure = MkNoStructure;
        matchEditStructure (TEditRepT repNoEdit repA) = do
        {
            MkEqualType <- matchWitnessKTT repNoEdit (typeRepKTT :: EditRepKTT NoEdit);
            MkEqualType <- matchWitnessT repA (typeRepT :: EditRepT a);
            return MkNoStructure;
        };
        matchEditStructure _ = Nothing;
    };

    data EitherStructure t where
    {
        MkEitherStructure :: forall ea' eb'. (Edit ea',Edit eb') => EitherStructure (Either ea' eb');
    };

    instance (Edit ea,Edit eb,Subject ea ~ Subject eb) => Edit (Either ea eb) where
    {
        type Subject (Either ea eb) = Subject ea;

        applyEdit (Left edit) = applyEdit edit;
        applyEdit (Right edit) = applyEdit edit;

        invertEdit (Left edit) s = fmap Left (invertEdit edit s);
        invertEdit (Right edit) s = fmap Right (invertEdit edit s);

{-
        data EditMatch (Either ea eb) t where
        {
            MkEitherMatch' :: forall ea' eb'. (Edit ea',Edit eb') => EditMatch (Either ea eb) (Either ea' eb');
        };
        eMatch1 = MkEitherMatch';
-}
        type EditStructure (Either ea eb) = EitherStructure;
        editStructure = MkEitherStructure;

        matchEditStructure (TEditRepT (TEditRepKTT repEither repEA) repEB) = do
        {
            MkEqualType <- matchWitnessKTKTT repEither (typeRepKTKTT :: EditRepKTKTT Either);
            MkEqualType <- matchWitnessT repEA (typeRepT :: EditRepT ea);
            MkEqualType <- matchWitnessT repEB (typeRepT :: EditRepT eb);
            return MkEitherStructure;
        };
        matchEditStructure _ = Nothing;
    };

    instance (FullEdit ea,Edit eb,Subject ea ~ Subject eb) => FullEdit (Either ea eb) where
    {
        replaceEdit s = Left (replaceEdit s);
    };
}
