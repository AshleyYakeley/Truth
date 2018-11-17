module Shapes.Import
    ( module I
    , module Shapes.Import
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
import Data.List as I ((++), intercalate, length, nub, zip)
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
    , Fractional(..)
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
import System.IO as I hiding (appendFile, getContents, hGetContents, interact, readFile, writeFile)
import Text.Read as I (Read(..), readMaybe)
import Text.Show as I (Show(..))

-- stm
import Control.Concurrent.STM as I

-- constraints
import Data.Constraint as I ((:-)(..), Dict(..))

-- mono-traversable
import Data.Containers as I
import Data.MonoTraversable as I
import Data.Sequences as I hiding (catMaybes, filter)

-- contravariant
import Data.Functor.Contravariant as I (Contravariant(..))

-- comonad
import Control.Comonad as I

-- lattices
import Algebra.Lattice as I

-- transformers
import Control.Monad.Trans.Class as I
import Control.Monad.Trans.Cont as I (ContT(..))
import Control.Monad.Trans.Except as I (ExceptT(..), runExceptT)
import Control.Monad.Trans.Identity as I (IdentityT(..))
import Control.Monad.Trans.Maybe as I (MaybeT(..))
import Control.Monad.Trans.Reader as I (ReaderT(..), ask, asks, local, withReaderT)
import Control.Monad.Trans.State as I (State, StateT(..), evalState, evalStateT, get, modify, put, runState)
import Control.Monad.Trans.Writer as I (Writer, WriterT(..), execWriterT, runWriter, tell)

-- transformers-extra
import Control.Monad.Trans.AskUnlift as I
import Control.Monad.Trans.Compose as I
import Control.Monad.Trans.Constraint as I
import Control.Monad.Trans.StackIO as I
import Control.Monad.Trans.Tunnel as I
import Control.Monad.Trans.Unlift as I

-- hashable
import Data.Hashable as I (Hashable)

-- containers
import Data.IntMap as I (IntMap, Key, traverseWithKey)
import qualified Data.Map.Lazy
import qualified Data.Map.Strict

-- unordered-containers
import Data.HashMap.Lazy as I (HashMap)

-- bytestring
import qualified Data.ByteString
import Data.ByteString.Lazy as I (appendFile, getContents, hGet, hGetContents, hPut, readFile, writeFile)
import qualified Data.ByteString.Lazy

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
import Data.Witness as I

-- open-witness
import Data.OpenWitness as I
import Data.Type.Heterogeneous as I

type LazyByteString = Data.ByteString.Lazy.ByteString

type StrictByteString = Data.ByteString.ByteString

type LazyMap = Data.Map.Lazy.Map

type StrictMap = Data.Map.Strict.Map

lastM :: [t] -> Maybe t
lastM [] = Nothing
lastM [t] = Just t
lastM (_:tt) = lastM tt
