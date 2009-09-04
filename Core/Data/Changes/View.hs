module Data.Changes.View where
{
    import Data.Changes.Object;
    import Data.Changes.EditScheme;

    data ViewResult w edit = MkViewResult
    {
        vrWidget :: w,
        vrUpdate :: edit -> IO ()
    };
    
    mapViewResult :: (w1 -> w2) -> ViewResult w1 edit -> ViewResult w2 edit;
    mapViewResult f (MkViewResult w1 u) = MkViewResult (f w1) u;

    type View w edit = Subject edit -> Push edit -> IO (ViewResult w edit);
    
    mapView :: (w1 -> w2) -> View w1 edit -> View w2 edit;
    mapView f view a push = fmap (mapViewResult f) (view a push);

    subscribeView :: View w edit -> Subscribe edit -> IO (Subscribe edit,w,IO ());
    subscribeView view subscribe = do
    {
        (vr,sub) <- subscribe view vrUpdate;
        return (subCopy sub,vrWidget vr,subClose sub);
    };
}
