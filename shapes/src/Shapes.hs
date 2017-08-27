module Shapes (module I) where
{

    import Shapes.Import as I;

    -- shapes
    import Data.Witness.All as I;
    import Data.WitnessStore as I;
    import Data.Store as I;
    import Data.Reducible as I;
    import Data.HasNewValue as I;
    import Data.Injection as I;
    import Data.Bijection as I;
    import Data.Chain as I;
    import Data.Lens as I;
    import Data.Codec as I;
    import Data.Result as I;
    import Data.Compose as I;
    import Data.MonadOne as I;
    import Data.Category as I;
    import Data.KindCategory as I;
    import Data.FiniteSet as I;
    import Control.Constrained.Category as I;
    import Control.Monad.Free as I;
    import Control.Monad.Tunnel as I;
    import Control.Monad.Trans.State.Extra as I hiding (liftCallCC,liftCatch);
    import Control.Monad.IsStateIO as I;
    import Data.KeyContainer as I;
}
