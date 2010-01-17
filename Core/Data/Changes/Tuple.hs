{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Changes.Tuple where
{
    import Data.Changes.Edit;
    import Data.Witness;
    import Data.ConstFunction;
    import Data.IsTuple;
    import Control.Category;
    import Prelude hiding (id,(.));

    data TupleEdit t where
    {
        MkTupleEdit :: forall edit. (Edit edit) => ListElementType (ListTuple t) (Subject edit) -> edit -> TupleEdit t;
    };

    modifyListElement :: ListElementType t a -> (a -> a) -> t -> t;
    modifyListElement el aa t = putListElement el (aa (getListElement el t)) t;

    instance (IsTuple t) => Edit (TupleEdit t) where
    {
        type Subject (TupleEdit t) = t;

        applyEdit (MkTupleEdit el edit) = FunctionConstFunction (
            fromListTuple . modifyListElement el (applyConstFunction (applyEdit edit)) . toListTuple
        );

        invertEdit (MkTupleEdit el edit) t = do
        {
            edit' <- invertEdit edit (getListElement el (toListTuple t));
            return (MkTupleEdit el edit');
        };
    };
}
