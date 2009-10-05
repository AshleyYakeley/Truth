module Data.Changes.Context where
{
    import Data.Result;
--    import Data.Changes.Tuple;
    import Data.Changes.FixedLens;
    import Data.Changes.SimpleLens;
    import Data.Changes.Edit;
    import Data.Changes.HasTypeRep;
    import Data.Changes.EditRep;
    import Data.OpenWitness;
    import Data.FunctorOne;
--    import Data.Witness;
    import Data.ConstFunction;
    import Data.Traversable;
    import Data.Foldable;
    import Control.Arrow;
    import Control.Applicative;
    import Control.Monad.Identity;

    data WithContext context content = MkWithContext context content;

    instance HasTypeRepKTKTT WithContext where
    {
        typeRepKTKTT = EditRepKTKTT (unsafeIOWitnessFromString "Data.Changes.Context.MkWithContext");
    };

    instance Functor (WithContext context) where
    {
        fmap ab (MkWithContext context a) = MkWithContext context (ab a);
    };

    instance Foldable (WithContext context) where
    {
        foldMap am (MkWithContext _ a) = am a;
    };

    instance Traversable (WithContext context) where
    {
        traverse afb (MkWithContext context a) = fmap (MkWithContext context) (afb a);
        sequenceA (MkWithContext context fa) = fmap (MkWithContext context) fa;
    };

    instance FunctorOne (WithContext context) where
    {
        retrieveOne (MkWithContext _ a) = SuccessResult a;
        getPureOne = arr (\(MkWithContext context _) content -> (MkWithContext context content));
    };
{-
    instance IsTuple (WithContext context content) where
    {
        type TList (WithContext context content) = (content,(context,()));
        fromListTuple (content,(context,())) = MkWithContext context content;
        toListTuple (MkWithContext context content) = (content,(context,()));
    };

    contentCleanLens :: (CompleteEditScheme content editn) =>
     CleanLens' Identity (WithContext context content) (TListEdit (content,(context,())) (editn,(editx,()))) content editn;
    contentCleanLens = tupleElementCleanLens HeadDoubleListElementType;

    contextCleanLens :: (CompleteEditScheme context editx) =>
     CleanLens' Identity (WithContext context content) (TListEdit (content,(context,())) (editn,(editx,()))) context editx;
    contextCleanLens = tupleElementCleanLens (TailDoubleListElementType HeadDoubleListElementType);
-}

    data ContextContentEdit editx editn = ReplaceContextContentEdit (WithContext (Subject editx) (Subject editn)) | ContextEdit editx | ContentEdit editn;

    instance HasTypeRepKTKTT ContextContentEdit where
    {
        typeRepKTKTT = EditRepKTKTT (unsafeIOWitnessFromString "Data.Changes.Context.ContextContentEdit");
    };

    instance (Edit editx, Edit editn) =>
     Edit (ContextContentEdit editx editn) where
    {
        type Subject (ContextContentEdit editx editn) = WithContext (Subject editx) (Subject editn);

        applyEdit (ContextEdit edit) = arr (\(MkWithContext x n) -> MkWithContext (applyConstFunction (applyEdit edit) x) n);
        applyEdit (ContentEdit edit) = arr (\(MkWithContext x n) -> MkWithContext x (applyConstFunction (applyEdit edit) n));
        applyEdit (ReplaceContextContentEdit a) = pure a;
        invertEdit (ContextEdit edit) (MkWithContext a _) = fmap ContextEdit (invertEdit edit a);
        invertEdit (ContentEdit edit) (MkWithContext _ a) = fmap ContentEdit (invertEdit edit a);
        invertEdit (ReplaceContextContentEdit _) a = Just (ReplaceContextContentEdit a);

        type EditEvidence (ContextContentEdit editx editn) = (EditInst editx,EditInst editn);
        editEvidence _ = (MkEditInst,MkEditInst);
    };

    instance (Edit editx, Edit editn) =>
     FullEdit (ContextContentEdit editx editn) where
    {
        replaceEdit = ReplaceContextContentEdit;
    };

    contextCleanLens :: CleanLens' Identity (ContextContentEdit editx editn) editx;
    contextCleanLens = MkCleanLens
    {
        cleanLensUpdate = \ccedit -> case ccedit of
        {
            ContextEdit edit -> Just edit;
            _ -> Nothing;
        },
        cleanLensSimple = MkSimpleLens
        {
            simpleLensGet = \(MkWithContext a _) -> a,
            simpleLensPutback = \a -> arr (\(MkWithContext _ x) -> Identity (MkWithContext a x))
        },
        cleanLensPutEdit = Identity . ContextEdit
    };

    contentCleanLens :: CleanLens' Identity (ContextContentEdit editx editn) editn;
    contentCleanLens = MkCleanLens
    {
        cleanLensUpdate = \ccedit -> case ccedit of
        {
            ContentEdit edit -> Just edit;
            _ -> Nothing;
        },
        cleanLensSimple = MkSimpleLens
        {
            simpleLensGet = \(MkWithContext _ a) -> a,
            simpleLensPutback = \a -> arr (\(MkWithContext x _) -> Identity (MkWithContext x a))
        },
        cleanLensPutEdit = Identity . ContentEdit
    };
}

