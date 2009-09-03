module Data.Changes.FloatingLens where
{
    import Data.Changes.JustEdit;
    import Data.Changes.EditScheme;
    import Data.FunctorOne;
    import Data.ConstFunction;
    import Data.Chain;
    import Control.Applicative;

    data FloatingLens' m state a edita b editb = MkFloatingLens
    {
        lensUpdate :: edita -> state -> ConstFunction a (state,Maybe editb),
        lensGet :: state -> a -> b,
        lensPutEdit :: state -> editb -> ConstFunction a (m edita)    -- m failure means impossible
    };
    
    type FloatingLens = FloatingLens' Maybe;
    
    toFloatingLens :: (FunctorOne m) => FloatingLens' m state a edita b editb -> FloatingLens state a edita b editb;
    toFloatingLens lens = MkFloatingLens
    {
        lensUpdate = lensUpdate lens,
        lensGet = lensGet lens,
        lensPutEdit = \state edit -> fmap getMaybeOne (lensPutEdit lens state edit)
    };

    -- suitable for Results, trying to put a failure code will be rejected 
    resultLens :: forall f state a edita b editb. (FunctorOne f,CompleteEditScheme a edita,CompleteEditScheme b editb) =>
     FloatingLens state a edita b editb -> FloatingLens state (f a) (JustEdit (f a) edita) (f b) (JustEdit (f b) editb);
    resultLens lens  = MkFloatingLens
    {
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
{-    
    instance CatFunctor (FloatingLens state) (Result err) where
    {
        cfmap = resultLens;
    };
    
    instance CatFunctor (FloatingLens state) Maybe where
    {
        cfmap = resultLens;
    };
-}
}
