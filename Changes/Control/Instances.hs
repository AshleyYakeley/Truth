{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Instances where
{
    import Control.Applicative;
    import Control.Monad.Error;
    
    instance (Monad m,Error err) => Applicative (ErrorT err m) where
    {
        pure = ErrorT . return . Right;
        
        (<*>) (ErrorT mep) (ErrorT meq) = ErrorT (do
        {
            ep <- mep;
            eq <- meq;
            return (do
            {
                p <- ep;
                q <- eq;
                return (p q);
            });
        });
    };
}
