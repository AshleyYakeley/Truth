module Shapes.Import
    ( module I
    , module Shapes.Import
    ) where

-- base
import Control.Applicative as I
import Control.Arrow as I hiding ((<<<), (>>>), (|||))
import Control.Category as I
import Control.Concurrent as I
import Control.Monad as I (Monad((>>), (>>=), return), MonadPlus(..), foldM, forever, unless, void, when)
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
import Data.List as I ((++), (\\), iterate, nub, nubBy, zip)
import Data.List qualified
import Data.List.NonEmpty as I (NonEmpty(..), head, init, last, nonEmpty, tail)
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

-- monadology
import Control.Monad.Ology as I hiding (Lens', cont, shift, state)

-- hashable
import Data.Hashable as I (Hashable)

-- containers
import Data.IntMap as I (IntMap, Key, traverseWithKey)
import Data.Map as I (Map)
import Data.Map.Lazy qualified

-- unordered-containers
import Data.HashMap.Lazy as I (HashMap)
import Data.HashSet as I (HashSet)

-- bytestring
import Data.ByteString as I (StrictByteString)
import Data.ByteString.Lazy as I
    ( LazyByteString
    , appendFile
    , getContents
    , hGet
    , hGetContents
    , hPut
    , readFile
    , writeFile
    )

-- vector
import Data.Vector as I (Vector, zipWith, zipWithM)

-- text
import Data.Text as I (Text)
import Data.Text.Encoding as I (decodeLatin1, decodeUtf8')
import Data.Text.Encoding
import Data.Text.Encoding.Error as I (UnicodeException(..))
import Data.Text.Encoding.Error

-- time
import Data.Time as I

-- random
import System.Random as I hiding (Finite)

-- invariant
import Data.Functor.Invariant as I

-- type-rig
import Data.TypeRig as I

-- countable
import Data.Countable as I
import Data.Empty as I
import Data.Searchable as I
import Data.Singular as I

-- witness
import Data.Type.Witness as I

-- open-witness
import Data.Type.OpenWitness as I
import Data.Type.OpenWitness.Witnessed as I

forn :: Applicative m => Int -> m a -> m [a]
forn 0 _ = pure []
forn n ma = liftA2 (:) ma $ forn (pred n) ma

liftComposeOuter :: (Functor f, Applicative g) => f a -> Compose f g a
liftComposeOuter fa = Compose $ fmap pure fa

liftComposeInner :: Applicative f => g a -> Compose f g a
liftComposeInner ga = Compose $ pure ga

decodeUtf8Lenient :: StrictByteString -> Text
decodeUtf8Lenient = decodeUtf8With lenientDecode

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

altIf :: Alternative m => Bool -> m ()
altIf False = empty
altIf True = pure ()

ifpure :: Alternative m => Bool -> a -> m a
ifpure False _ = empty
ifpure True x = pure x

mpure :: Alternative m => Maybe a -> m a
mpure (Just a) = pure a
mpure Nothing = empty

lpure :: Alternative m => [a] -> m a
lpure [] = empty
lpure (a:aa) = pure a <|> lpure aa

mcatch :: Alternative m => m a -> m (Maybe a)
mcatch ma = fmap Just ma <|> pure Nothing

altIs :: Alternative m => m a -> m Bool
altIs ma = fmap (\_ -> True) ma <|> pure False

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

shortOr :: Monad m => [m Bool] -> m Bool
shortOr [] = return False
shortOr (mb:mbb) = do
    b <- mb
    if b
        then return True
        else shortOr mbb

shortAnd :: Monad m => [m Bool] -> m Bool
shortAnd [] = return True
shortAnd (mb:mbb) = do
    b <- mb
    if b
        then shortAnd mbb
        else return False

shortFirst :: Monad m => [m (Maybe a)] -> m (Maybe a)
shortFirst [] = return Nothing
shortFirst (mma:aa) = do
    ma <- mma
    case ma of
        Just a -> return $ Just a
        Nothing -> shortFirst aa

forFirst :: Monad m => [a] -> (a -> m (Maybe b)) -> m (Maybe b)
forFirst l f = shortFirst $ fmap f l

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

concatmap :: Monoid b => (a -> b) -> [a] -> b
concatmap f xx = mconcat $ fmap f xx

intercalate1 :: Monoid a => a -> NonEmpty a -> a
intercalate1 i (a1 :| aa) = a1 <> concatmap (\a -> i <> a) aa

intercalate :: Monoid a => a -> [a] -> a
intercalate i aa =
    case nonEmpty aa of
        Just aa1 -> intercalate1 i aa1
        Nothing -> mempty

startsWith :: Eq a => [a] -> [a] -> Maybe [a]
startsWith [] s = Just s
startsWith (p:pp) (q:qq)
    | p == q = startsWith pp qq
startsWith _ _ = Nothing

endsWith :: Eq a => [a] -> [a] -> Maybe [a]
endsWith e s = do
    a <- startsWith (reverse e) (reverse s)
    return $ reverse a

replaceListFirst :: Eq a => [a] -> [a] -> [a] -> Maybe [a]
replaceListFirst needle replacement haystack
    | Just hh <- startsWith needle haystack = Just $ replacement <> hh
replaceListFirst _ _ [] = Nothing
replaceListFirst needle replacement (h:hh) = do
    hh' <- replaceListFirst needle replacement hh
    return $ h : hh'

replaceListAll :: Eq a => [a] -> [a] -> [a] -> [a]
replaceListAll needle replacement haystack
    | Just hh <- startsWith needle haystack = replacement <> replaceListAll needle replacement hh
replaceListAll _ _ [] = []
replaceListAll needle replacement (h:hh) = h : replaceListAll needle replacement hh

trimSpace ::
       forall seq. (IsSequence seq, Element seq ~ Char)
    => seq
    -> seq
trimSpace = reverse . dropWhile isSpace . reverse . dropWhile isSpace

lifecycleOnAllDone ::
       forall m. MonadAskUnliftIO m
    => m ()
    -> m (LifecycleT m (), m ())
lifecycleOnAllDone onzero = do
    var <- liftIO $ newMVar (0 :: Int)
    let
        ondone = do
            liftIO $
                mVarRunStateT var $ do
                    olda <- get
                    put $ succ olda
            lifecycleOnClose $ do
                iszero <-
                    mVarRunStateT var $ do
                        olda <- get
                        let newa = pred olda
                        put newa
                        return $ newa == 0
                if iszero
                    then onzero
                    else return ()
        checkdone = do
            iszero <-
                mVarRunStateT var $ do
                    a <- get
                    return $ a == 0
            if iszero
                then onzero
                else return ()
    return (ondone, checkdone)

threadSleep :: NominalDiffTime -> IO ()
threadSleep d = threadDelay $ truncate $ (nominalDiffTimeToSeconds d) * 1E6

liftWithFinal ::
       forall m1 m2 a. Monad m2
    => (m1 --> m2)
    -> m1 (m2 a)
    -> m2 a
liftWithFinal lft call = do
    ma <- lft call
    ma

liftWithDefer ::
       forall m1 m2 a. (MonadIO m1, MonadIO m2)
    => (m1 --> m2)
    -> ((m2 () -> m1 ()) -> m1 a)
    -> m2 a
liftWithDefer lft call = do
    var <- liftIO $ newMVar (return ())
    a <- lft $ call $ \tmu -> liftIO $ modifyMVar_ var $ \oldval -> return $ oldval >> tmu
    tmu <- liftIO $ takeMVar var
    tmu
    return a
