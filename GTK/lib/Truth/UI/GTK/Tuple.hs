{-# OPTIONS -fno-warn-orphans #-}
module Truth.UI.GTK.Tuple(tupleTypeKnowledge) where
{
    import Data.Proxy;
    import Data.Foldable;
    import Data.Functor.Identity;
    import Graphics.UI.Gtk;
    import Data.KindCategory;
    import Data.Reity;
    import Truth.Core;
    import Truth.UI.GTK.GView;


    arrangeWidgets :: [Widget] -> IO Widget;
    arrangeWidgets widgets = do
    {
        vbox <- vBoxNew False 0;
        for_ widgets $ \widget -> boxPackEnd vbox widget PackNatural 0;
        return $ toWidget vbox;
    };

    tupleGView :: (FiniteTupleSelector sel,Applicative m) => (forall edit. sel edit -> m (GView edit)) -> m (GView (TupleEdit sel));
    tupleGView selview = fmap (mapIOView arrangeWidgets) $ tupleView selview;

    instance (FiniteTupleSelector sel,TupleHasInfo sel) => DependentHasGView (TupleEdit sel) where
    {
        dependsGView k _i = tupleGView $ \sel -> case tupleWitness (Proxy :: Proxy Edit) sel of
        {
            MkConstraintWitness -> namedResult "selector" $ getGView k $ tupleHasInfo sel;
        };
    };

    instance (FiniteTupleSelector sel,TupleHasInfo sel,TupleWitness HasGView sel) => HasGView (TupleEdit sel) where
    {
        gview = runIdentity $ tupleGView $ \sel -> case tupleWitness (Proxy :: Proxy HasGView) sel of
        {
            MkConstraintWitness -> Identity gview;
        };
    };

    tupleTypeKnowledge :: TypeKnowledge;
    tupleTypeKnowledge = namedKnowledge "tuple" $(declInfo [d|
        instance (FiniteTupleSelector sel,TupleHasInfo sel) => DependentHasGView (TupleEdit sel);
    |]);
}
