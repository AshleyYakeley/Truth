module Data.Changes.View where
{
    import Data.Changes.Object;
    import Data.Changes.FloatingLens;
    import Data.Changes.Edit;
    import Data.Changes.HasNewValue;
    import Data.TypeKT;

    data Selection edit where
    {
        MkSelection ::
         forall editb state. (Eq state,HasNewValue (Subject editb),FullEdit editb) =>
          InfoT editb -> InfoT (Subject editb) -> FloatingLens state edita editb -> Selection edita;
    };

    data ViewWidgetStuff w edit = MkViewWidgetStuff
    {
        vwsWidget :: w,
        vwsGetSelection :: IO (Maybe (Selection edit))
    };

    data ViewResult w edit = MkViewResult
    {
        vrWidgetStuff :: ViewWidgetStuff w edit,
        vrUpdate :: edit -> IO ()
    };

    type View w edit = Subject edit -> Push edit -> IO (ViewResult w edit);

    subscribeView :: View w edit -> Subscribe edit -> IO (Subscribe edit,ViewWidgetStuff w edit,IO ());
    subscribeView view subscribe = do
    {
        (vr,sub) <- subscribe view vrUpdate;
        return (subCopy sub,vrWidgetStuff vr,subClose sub);
    };
}
