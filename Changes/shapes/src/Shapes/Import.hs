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
import Data.Char as I hiding (toLower, toTitle, toUpper)
import Data.Coerce as I
import Data.Either as I
import Data.Eq as I
import Data.Foldable as I hiding (find)
import Data.Functor.Compose as I
import Data.Functor.Identity as I
import Data.Functor.Product as I
import Data.Int as I
import Data.Kind as I
import Data.List as I ((++), iterate, nub, nubBy, zip)
import qualified Data.List
import Data.List.NonEmpty as I (NonEmpty(..), last, nonEmpty)
import Data.Maybe as I hiding (catMaybes, mapMaybe)
import Data.Monoid as I (Monoid(..))
import Data.Ord as I
import Data.Semigroup as I hiding (Product(..))
import Data.String as I hiding (lines, unlines, unwords, words)
import Data.Traversable as I
import Data.Tuple as I
import Data.Unique as I
import Data.Void as I
import Data.Word as I
import GHC.Stack as I (HasCallStack)
import Prelude as I
    ( Bounded(..)
    , Enum(..)
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
import Text.ParserCombinators.ReadPrec as I (ReadPrec)
import Text.Read as I (Read(..), readMaybe)
import Text.Show as I (Show(..))

-- stm
import Control.Concurrent.STM as I

-- constraints
import Data.Constraint as I ((:-)(..), Dict(..), withDict)

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

-- monadology
import Control.Monad.Ology.ComposeInner as I
import Control.Monad.Ology.ComposeOuter as I
import Control.Monad.Ology.Data as I hiding (Lens')
import Control.Monad.Ology.Exception as I
import Control.Monad.Ology.Function as I
import Control.Monad.Ology.Inner as I
import Control.Monad.Ology.Outer as I
import Control.Monad.Ology.Result as I
import Control.Monad.Ology.Trans.AskUnlift as I
import Control.Monad.Ology.Trans.Compose as I
import Control.Monad.Ology.Trans.Constraint as I
import Control.Monad.Ology.Trans.ContExtra as I
import Control.Monad.Ology.Trans.ReaderState as I
import Control.Monad.Ology.Trans.Stack as I
import Control.Monad.Ology.Trans.Tunnel as I
import Control.Monad.Ology.Trans.Unlift as I
import Control.Monad.Ology.Transform as I

-- hashable
import Data.Hashable as I (Hashable)

-- containers
import Data.IntMap as I (IntMap, Key, traverseWithKey)
import Data.Map as I (Map)
import qualified Data.Map.Lazy

-- unordered-containers
import Data.HashMap.Lazy as I (HashMap)
import Data.HashSet as I (HashSet)

-- bytestring
import qualified Data.ByteString
import Data.ByteString.Lazy as I (appendFile, getContents, hGet, hGetContents, hPut, readFile, writeFile)
import qualified Data.ByteString.Lazy

-- vector
import Data.Vector as I (Vector)

-- cereal
import Data.Serialize as I (Serialize)

-- text
import Data.Text as I (Text, strip)
import Data.Text.Encoding as I (decodeUtf8')
import Data.Text.Encoding.Error as I (UnicodeException(..))

-- cereal-text
import Data.Serialize.Text as I ()

-- random
import System.Random as I hiding (Finite)

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

ifpure :: Alternative m => Bool -> a -> m a
ifpure False _ = empty
ifpure True x = pure x

mpure :: Alternative m => Maybe a -> m a
mpure (Just a) = pure a
mpure Nothing = empty

mcatch :: Alternative m => m a -> m (Maybe a)
mcatch ma = fmap Just ma <|> pure Nothing

choice :: Alternative m => [m a] -> m a
choice = foldr (<|>) empty

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

-- | O(n^2) rather than O(n log n), due to no Ord constraint
duplicates :: Eq a => [a] -> [a]
duplicates [] = []
duplicates (a:aa) =
    if elem a aa
        then a : duplicates (Data.List.filter (/= a) aa)
        else duplicates aa

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

localf :: (Traversable f, Applicative m) => (r -> f r) -> ReaderT r m a -> ReaderT r m (f a)
localf rfr (ReaderT rma) = ReaderT $ \r -> for (rfr r) rma

maybePoint :: (Monoid l, MonoPointed l, Element l ~ a) => Maybe a -> l
maybePoint Nothing = mempty
maybePoint (Just a) = opoint a

isSubsetOf :: SetContainer t => t -> t -> Bool
isSubsetOf a b = onull $ difference a b

deletesMap :: IsMap map => [ContainerKey map] -> map -> map
deletesMap [] = id
deletesMap (k:kk) = deleteMap k . deletesMap kk

intercalate :: Monoid a => a -> [a] -> a
intercalate _ [] = mempty
intercalate _ [a] = a
intercalate i (a:aa) = mconcat [a, i, intercalate i aa]

startsWith :: Eq a => [a] -> [a] -> Maybe [a]
startsWith [] s = Just s
startsWith (p:pp) (q:qq)
    | p == q = startsWith pp qq
startsWith _ _ = Nothing

endsWith :: Eq a => [a] -> [a] -> Maybe [a]
endsWith e s = do
    a <- startsWith (reverse e) (reverse s)
    return $ reverse a
