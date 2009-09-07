{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Changes.Tuple where
{
{-
    import Data.Changes.FixedLens;
    import Data.Changes.Edit;
    import Data.Witness;
    import Data.ConstFunction;
    import Control.Monad.Identity;
    import Control.Category;
    import Prelude hiding (id,(.));

    class (Is (ListType Type) (TList a)) => IsTuple a where
    {
        type TList a;
        fromListTuple :: TList a -> a;
        toListTuple :: a -> TList a;
    };

    instance IsTuple (a,b) where
    {
        type TList (a,b) = (a,(b,()));
        fromListTuple (a,(b,())) = (a,b);
        toListTuple (a,b) = (a,(b,()));
    };

    instance IsTuple (a,b,c) where
    {
        type TList (a,b,c) = (a,(b,(c,())));
        fromListTuple (a,(b,(c,()))) = (a,b,c);
        toListTuple (a,b,c) = (a,(b,(c,())));
    };

    data DoubleListElementType la a lb b where
    {
        HeadDoubleListElementType :: DoubleListElementType (a,ar) a (b,br) b;
        TailDoubleListElementType :: DoubleListElementType la a lb b -> DoubleListElementType (ah,la) a (bh,lb) b;
    };

    matchDoubleListElementType :: forall la a1 a2 lb b1 b2. DoubleListElementType la a1 lb b1 -> DoubleListElementType la a2 lb b2 ->
     Maybe (EqualType a1 a2,EqualType b1 b2);
    matchDoubleListElementType HeadDoubleListElementType HeadDoubleListElementType = Just (MkEqualType,MkEqualType);
    matchDoubleListElementType (TailDoubleListElementType et1) (TailDoubleListElementType et2) = do
    {
        (MkEqualType :: EqualType a1 a2,MkEqualType :: EqualType b1 b2) <- matchDoubleListElementType1 et1 et2;
        return (MkEqualType,MkEqualType);
    };
    matchDoubleListElementType1 _ _ = Nothing;

    getDoubleListElement1 :: DoubleListElementType la a lb b -> la -> a;
    getDoubleListElement1 HeadDoubleListElementType (a,_) = a;
    getDoubleListElement1 (TailDoubleListElementType et) (_,la) = getDoubleListElement1 et la;
    
    putDoubleListElement1 :: DoubleListElementType la a lb b -> a -> la -> la;
    putDoubleListElement1 HeadDoubleListElementType a (_,ar) = (a,ar);
    putDoubleListElement1 (TailDoubleListElementType et) a (ah,la) = (ah,putDoubleListElement1 et a la);

    data TListEdit t editt where
    {
        ReplaceTListEdit :: t -> TListEdit t editt;
        TListEdit :: forall a edit. (EditScheme a edit) => DoubleListElementType t a editt edit -> edit -> TListEdit t editt;
    };

    applyTupleEdit :: TListEdit t edits -> t -> t;
    applyTupleEdit (ReplaceTListEdit t) _ = t;
    applyTupleEdit (TListEdit n edit) t = putDoubleListElement1 n (applyConstFunction (applyEdit edit) (getDoubleListElement1 n t)) t;

    invertTupleEdit :: TListEdit t edits -> t -> Maybe (TListEdit t edits);
    invertTupleEdit (ReplaceTListEdit _) t = Just (ReplaceTListEdit t);
    invertTupleEdit (TListEdit n edit) t = do
    {
        unedit <- invertEdit edit (getDoubleListElement1 n t);
        return (TListEdit n unedit);
    };

    instance (IsTuple t,tl ~ TList t) => EditScheme t (TListEdit tl edits) where
    {
        --applyPartEdit :: TListPartEdit pet (TList x) -> ConstFunction x x;
        applyEdit edit = FunctionConstFunction (fromListTuple . (applyTupleEdit edit) . toListTuple);

        --invertPartEdit :: TListPartEdit pet (TList x) -> x -> Maybe (Edit' (TListPartEdit pet (TList x)) x);    -- "Nothing" means no change
        invertEdit edit x = invertTupleEdit edit (toListTuple x);
    };

    instance (IsTuple t,tl ~ TList t) => CompleteEditScheme t (TListEdit tl edits) where
    {
        replaceEdit = ReplaceTListEdit . toListTuple;
    };
    
    tupleElementCleanLens :: (IsTuple t,CompleteEditScheme a edit,EditScheme t (TListEdit (TList t) edits)) =>
     DoubleListElementType (TList t) a edits edit -> CleanLens' Identity t (TListEdit (TList t) edits) a edit;
    tupleElementCleanLens n = MkCleanLens
    {
        cleanLensUpdate = \edit -> case edit of
        {
            TListEdit n' edita -> do
            {
                (MkEqualType,MkEqualType) <- matchDoubleListElementType n n';
                return edita;
            };
            ReplaceTListEdit t -> Just (replaceEdit (getDoubleListElement1 n t));
        },
        cleanLensGet = (getDoubleListElement1 n) . toListTuple,
        cleanLensPutEdit = \editb -> Identity (TListEdit n editb)
    };
-}
}
