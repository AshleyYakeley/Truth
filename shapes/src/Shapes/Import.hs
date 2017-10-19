module Shapes.Import
    ( module I
    ) where

-- base
import Control.Applicative as I
import Control.Arrow as I hiding ((<<<), (>>>), (|||))
import Control.Category as I
import Control.Concurrent as I
import Control.Exception as I hiding (catch)
import Control.Monad as I (Functor(..), Monad((>>), (>>=), return), MonadPlus(..))
import Control.Monad.Fail as I
import Control.Monad.Fix as I
import Control.Monad.IO.Class as I
import Data.Bits as I
import Data.Bool as I
import Data.Char as I hiding (toLower, toUpper)
import Data.Either as I
import Data.Eq as I
import Data.Foldable as I hiding (find)
import Data.Functor.Compose as I
import Data.Functor.Identity as I
import Data.Int as I
import Data.Kind as I
import Data.List as I ((++), intercalate, length, zip)
import Data.List.NonEmpty as I (NonEmpty(..))
import Data.Maybe as I hiding (catMaybes, mapMaybe)
import Data.Monoid as I (Monoid(..))
import Data.Ord as I
import Data.Semigroup as I hiding (All(..), Any(..))
import Data.String as I hiding (lines, unlines, unwords, words)
import Data.Traversable as I
import Data.Tuple as I
import Data.Word as I
import Prelude as I
    ( Double
    , Enum(..)
    , Integer
    , Integral(..)
    , Num(..)
    , Real(..)
    , ($)
    , const
    , error
    , fromInteger
    , fromIntegral
    , seq
    , toInteger
    , undefined
    )
import System.IO as I
import Text.Read as I (Read(..), readMaybe)
import Text.Show as I (Show(..))

-- constraints
import Data.Constraint as I ((:-)(..), Dict(..))

-- mono-traversable
import Data.Containers as I
import Data.MonoTraversable as I
import Data.Sequences as I hiding (catMaybes, filter)

-- comonad
import Control.Comonad as I

-- lattices
import Algebra.Lattice as I

-- transformers
import Control.Monad.Trans.Class as I
import Control.Monad.Trans.Cont as I (ContT(..))
import Control.Monad.Trans.Except as I (ExceptT(..), runExceptT)
import Control.Monad.Trans.Identity as I (IdentityT(..))
import Control.Monad.Trans.List as I (ListT(..))
import Control.Monad.Trans.Maybe as I (MaybeT(..))
import Control.Monad.Trans.Reader as I (ReaderT(..))
import Control.Monad.Trans.State as I (StateT(..), evalStateT, get, put)
import Control.Monad.Trans.Writer as I (WriterT(..), execWriterT, tell)

-- hashable
import Data.Hashable as I (Hashable)

-- containers
import Data.IntMap as I (IntMap, Key, traverseWithKey)

-- unordered-containers
import Data.HashMap.Lazy as I (HashMap)

-- bytestring
import Data.ByteString.Lazy as I (ByteString, hGet, hPut)

-- cereal
import Data.Serialize as I (Serialize)

-- text
import Data.Text as I (Text)

-- cereal-text
import Data.Serialize.Text as I ()

-- random
import System.Random as I

-- countable
import Data.Countable as I
import Data.Empty as I
import Data.Searchable as I

-- witness
import Data.Witness as I hiding (EitherWitness(..))

-- open-witness
import Data.OpenWitness as I
import Data.Type.Heterogeneous as I
