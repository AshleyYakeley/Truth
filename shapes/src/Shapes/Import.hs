module Shapes.Import
    ( module I
    , module Shapes.Import
    ) where

-- base
import Control.Applicative as I
import Control.Arrow as I hiding ((<<<), (>>>), (|||))
import Control.Category as I
import Control.Concurrent as I
import Control.Monad as I (Monad((>>), (>>=), return), MonadPlus(..), forever, void)
import Control.Monad.Fail as I
import Control.Monad.Fix as I
import Control.Monad.IO.Class as I
import Data.Bits as I
import Data.Bool as I
import Data.Char as I hiding (toLower, toUpper)
import Data.Coerce as I
import Data.Either as I
import Data.Eq as I
import Data.Foldable as I hiding (find)
import Data.Functor.Compose as I
import Data.Functor.Identity as I
import Data.Int as I
import Data.Kind as I
import Data.List as I ((++), intercalate, nub, nubBy, zip)
import Data.List.NonEmpty as I (NonEmpty(..), last, nonEmpty)
import Data.Maybe as I hiding (catMaybes, mapMaybe)
import Data.Monoid as I (Monoid(..))
import Data.Ord as I
import Data.Semigroup as I
import Data.String as I hiding (lines, unlines, unwords, words)
import Data.Traversable as I
import Data.Tuple as I
import Data.Unique as I
import Data.Word as I
import GHC.Stack as I (HasCallStack)
import Prelude as I
    ( Enum(..)
    , Integer
    , Integral(..)
    , Num(..)
    , Real(..)
    , RealFrac(..)
    , ($)
    , (^)
    , (^^)
    , const
    , error
    , even
    , fromInteger
    , fromIntegral
    , gcd
    , lcm
    , odd
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
import Control.Monad.Trans.Cont as I (ContT(..), evalContT, mapContT)
import Control.Monad.Trans.Except as I (ExceptT(..), runExceptT)
import Control.Monad.Trans.Identity as I (IdentityT(..))
import Control.Monad.Trans.Maybe as I (MaybeT(..))
import Control.Monad.Trans.Reader as I (ReaderT(..), ask, asks, local, withReaderT)
import Control.Monad.Trans.State as I (State, StateT(..), evalState, evalStateT, get, modify, put, runState)
import Control.Monad.Trans.Writer as I (Writer, WriterT(..), execWriter, execWriterT, listen, runWriter, tell)

-- transformers-extra
import Control.Monad.Trans.AskUnlift as I
import Control.Monad.Trans.Compose as I
import Control.Monad.Trans.Constraint as I
import Control.Monad.Trans.ContExtra as I
import Control.Monad.Trans.Function as I
import Control.Monad.Trans.ReaderState as I
import Control.Monad.Trans.Stack as I
import Control.Monad.Trans.Tunnel as I
import Control.Monad.Trans.Unlift as I

-- hashable
import Data.Hashable as I (Hashable)

-- containers
import Data.IntMap as I (IntMap, Key, traverseWithKey)
import Data.Map as I (Map)
import qualified Data.Map.Lazy

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
import Control.Category.Tensor as I
import Data.Witness as I

-- open-witness
import Data.OpenWitness as I
import Data.OpenWitness.Order as I
import Data.OpenWitness.Witnessed as I

type LazyByteString = Data.ByteString.Lazy.ByteString

type StrictByteString = Data.ByteString.ByteString

insertMapLazy :: Ord k => k -> v -> Map k v -> Map k v
insertMapLazy = Data.Map.Lazy.insert

lastM :: [t] -> Maybe t
lastM [] = Nothing
lastM [t] = Just t
lastM (_:tt) = lastM tt

eitherLeft :: Either a b -> Maybe a
eitherLeft (Left x) = Just x
eitherLeft (Right _) = Nothing

eitherRight :: Either a b -> Maybe b
eitherRight (Left _) = Nothing
eitherRight (Right x) = Just x

mpure :: Alternative m => Maybe a -> m a
mpure (Just a) = pure a
mpure Nothing = empty

mcatch :: Alternative m => m a -> m (Maybe a)
mcatch ma = fmap Just ma <|> pure Nothing

compAll :: Category cat => [cat a a] -> cat a a
compAll [] = id
compAll (c:cc) = c . compAll cc

exec :: Monad m => m (m a) -> m a
exec mma = mma >>= id

deleteFirstMatching :: (a -> Bool) -> [a] -> [a]
deleteFirstMatching _ [] = []
deleteFirstMatching t (a:aa)
    | t a = aa
deleteFirstMatching t (a:aa) = a : deleteFirstMatching t aa

mFindIndex ::
       forall m a. Monad m
    => (a -> m Bool)
    -> [a]
    -> m (Maybe Int)
mFindIndex test = let
    findI :: Int -> [a] -> m (Maybe Int)
    findI _ [] = return Nothing
    findI i (a:aa) = do
        t <- test a
        if t
            then return (Just i)
            else findI (succ i) aa
    in findI 0

shortOr :: Monad m => (a -> m Bool) -> [a] -> m Bool
shortOr _ [] = return False
shortOr amb (a:aa) = do
    b <- amb a
    if b
        then return True
        else shortOr amb aa

shortAnd :: Monad m => (a -> m Bool) -> [a] -> m Bool
shortAnd _ [] = return True
shortAnd amb (a:aa) = do
    b <- amb a
    if b
        then shortAnd amb aa
        else return False

mif :: Monoid a => Bool -> a -> a
mif False _ = mempty
mif True a = a
