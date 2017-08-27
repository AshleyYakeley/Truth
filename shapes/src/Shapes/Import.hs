module Shapes.Import (module I) where
{
    -- base
    import Prelude as I (($),const,seq,error,undefined,Eq(..),Ord(..),Enum(..),Num(..),Real(..),Integral(..),fromIntegral,fromInteger,toInteger,Double);
    import Data.Kind as I;
    import Data.Bool as I;
    import Data.Bits as I;
    import Data.List as I ((++),length,intercalate,zip);
    import Data.List.NonEmpty as I (NonEmpty(..));
    import Data.Maybe as I hiding (catMaybes,mapMaybe);
    import Data.Either as I;
    import Data.Ord as I;
    import Data.Int as I;
    import Data.Char as I hiding (toLower,toUpper);
    import Data.String as I hiding (words,unwords,lines,unlines);
    import Data.Word as I;
    import Data.Tuple as I;
    import Data.Foldable as I hiding (find);
    import Data.Traversable as I;
    import Data.Semigroup as I hiding (Any(..),All(..));
    import Data.Monoid as I (Monoid(..));
    import Control.Applicative as I;
    import Data.Functor.Identity as I;
    import Data.Functor.Compose as I;
    import Control.Monad as I (Functor(..), Monad(return,(>>=),(>>)), MonadPlus(..));
    import Control.Monad.Fail as I;
    import Control.Monad.Fix as I;
    import Control.Monad.IO.Class as I;
    import Control.Category as I;
    import Control.Arrow as I hiding ((|||),(<<<),(>>>));
    import Control.Concurrent.MVar as I;
    import Text.Show as I (Show(..));
    import Control.Exception as I hiding (catch);
    import System.IO as I;
    import Control.Concurrent as I;

    -- mono-traversable
    import Data.MonoTraversable as I;
    import Data.Containers as I;
    import Data.Sequences as I hiding(catMaybes,filter);

    -- comonad
    import Control.Comonad as I;

    -- lattices
    import Algebra.Lattice as I;

    -- transformers
    import Control.Monad.Trans.Class as I;
    import Control.Monad.Trans.Reader as I (ReaderT(..));
    import Control.Monad.Trans.Writer as I (WriterT(..),execWriterT,tell);
    import Control.Monad.Trans.State as I (StateT(..),evalStateT,get,put);

    -- hashable
    import Data.Hashable as I (Hashable);

    -- containers
    import Data.IntMap as I (IntMap,Key,traverseWithKey);

    -- unordered-containers
    import Data.HashMap.Lazy as I (HashMap);

    -- bytestring
    import Data.ByteString.Lazy as I (ByteString,hGet,hPut);

    -- text
    import Data.Text as I (Text);

    -- random
    import System.Random as I;

    -- countable
    import Data.Empty as I;
    import Data.Searchable as I;
    import Data.Countable as I;

    -- witness
    import Data.Witness as I hiding (EitherWitness(..));

    -- open-witness
    import Data.Type.Heterogeneous as I;
    import Data.OpenWitness as I;
}
