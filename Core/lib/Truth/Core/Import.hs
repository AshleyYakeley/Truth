module Truth.Core.Import (module I) where
{
    import Prelude as I (($),seq,error,undefined,Eq(..),Ord(..),Enum(..),Num(..),Real(..),Integral(..),fromIntegral,fromInteger,toInteger);
    import Data.Kind as I;
    import Data.Bool as I;
    import Data.Bits as I;
    import Data.List as I ((++),length);
    import Data.List.NonEmpty as I (NonEmpty(..));
    import Data.Maybe as I hiding (catMaybes);
    import Data.Either as I;
    import Data.Ord as I;
    import Data.Int as I;
    import Data.Char as I hiding (toLower,toUpper);
    import Data.String as I hiding (words,unwords,lines,unlines);
    import Data.Word as I;
    import Data.Tuple as I;
    import Data.Monoid as I hiding (Any(..));
    import Data.Functor.Identity as I;
    import Control.Monad.Trans.State as I (StateT(..),evalStateT,get,put);
    import Control.Category as I;
    import Control.Monad as I (Functor(..),Monad(..));
    import Control.Arrow as I hiding ((|||),(<<<),(>>>));
    import Data.Foldable as I hiding (find);
    import Data.Traversable as I;
    import Control.Applicative as I;
    import Data.Functor.Constant as I;
    import Control.Monad.IO.Class as I;
    import Control.Monad.Trans.Class as I;
    import Control.Monad.Trans.Writer as I;
    import Control.Monad.Fix as I;
    import Control.Concurrent.MVar as I;
    import Text.Show as I (Show(..));
    import System.IO as I;
    import Control.Exception as I hiding (catch);

    import Data.ByteString as I (ByteString,hPut);
    import Data.MonoTraversable as I;
    import Data.Sequences as I;
    import Data.Containers as I;
    import Data.KeyContainer as I;
    import Control.Comonad as I;

    import Data.Type.Heterogeneous as I;
    import Data.Witness as I;
    import Data.OpenWitness as I;

    import Data.Searchable as I;
    import Data.Countable as I;
    import Data.WitnessStore as I;
    import Data.Store as I;
    import Data.HasNewValue as I;
    import Data.Injection as I;
    import Data.Bijection as I;
    import Data.Empty as I;
    import Data.Chain as I;
    import Data.FloatingLens as I;
    import Data.Lens as I;
    import Data.Codec as I;
    import Data.Result as I;
    import Data.Compose as I;
    import Data.MonadOne as I;
    import Data.Category as I;
    import Data.KindCategory as I;
    import Control.Monad.Free as I;
    import Control.Monad.Tunnel as I;
    import Control.Monad.Trans.State.Extra as I hiding (liftCallCC,liftCatch);
    import Control.Monad.IsStateIO as I;
    import Data.Reity as I;
}
