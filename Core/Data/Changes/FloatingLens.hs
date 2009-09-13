module Data.Changes.FloatingLens where
{
    import Data.Changes.JustEdit;
    import Data.Changes.Edit;
    import Data.FunctorOne;
    import Data.ConstFunction;
    import Data.Chain;
    import Control.Applicative;

    data FloatingLens' m state edita editb = MkFloatingLens
    {
        lensInitial :: state,
        lensUpdate :: edita -> state -> ConstFunction (Subject edita) (state,Maybe editb),
        lensGet :: state -> Subject edita -> Subject editb,
        lensPutEdit :: state -> editb -> ConstFunction (Subject edita) (m edita)    -- m failure means impossible
    };
    
    type FloatingLens = FloatingLens' Maybe;
    
    toFloatingLens :: (FunctorOne m) => FloatingLens' m state edita editb -> FloatingLens state edita editb;
    toFloatingLens lens = MkFloatingLens
    {
        lensInitial = lensInitial lens,
        lensUpdate = lensUpdate lens,
        lensGet = lensGet lens,
        lensPutEdit = \state edit -> fmap getMaybeOne (lensPutEdit lens state edit)
    };

    -- suitable for Results, trying to put a failure code will be rejected 
    resultLens :: forall f state edita editb. (FunctorOne f,FullEdit edita,FullEdit editb) =>
     FloatingLens state edita editb -> FloatingLens state (JustEdit f edita) (JustEdit f editb);
    resultLens lens  = MkFloatingLens
    {
        lensInitial = lensInitial lens,
        lensUpdate = \editfa state -> case extractJustEdit editfa of
        {
            Just edita -> do
            {
                msmeb <-  cofmap1CF getMaybeOne (cfmap (lensUpdate lens edita state));
                return (case msmeb of
                {
                    Just (newstate,meditb) -> (newstate,fmap JustEdit meditb);
                    Nothing -> (state,Nothing);
                });
            };
            Nothing -> pure (state,Nothing);
        },
        lensGet = \state -> fmap (lensGet lens state),
        lensPutEdit = \state editfb -> case extractJustEdit editfb of
        {
            Just editb -> do
            {
                mmea <- cofmap1CF getMaybeOne (cfmap (lensPutEdit lens state editb));
                return (case mmea of
                {
                    Just (Just edita) -> Just (JustEdit edita);
                    _ -> Nothing;
                });
            };
            Nothing -> pure Nothing;
        }
    };
}
