module Data.Streamable where

import Text.ParserCombinators.ReadP qualified as ReadP
import Text.ParserCombinators.ReadPrec qualified as ReadPrec

import Data.Filterable
import Shapes.Import

class (Riggable f, Monoid (StreamableBasis f)) => Streamable f where
    type StreamableBasis f :: Type
    rItem :: f (Element (StreamableBasis f))
    rWhole :: f (StreamableBasis f)
    rLiterals :: StreamableBasis f -> f ()
    rLiteral :: Element (StreamableBasis f) -> f ()
    rExact ::
        forall a.
        Eq a =>
        a ->
        f a ->
        f ()

class (MonadPlus m, Streamable m) => Readish m where
    (<++) :: forall a. m a -> m a -> m a

rMaybe :: forall m a. Readish m => m a -> m (Maybe a)
rMaybe p = fmap Just p <++ pure Nothing

rMany :: forall m a. Readish m => m a -> m [a]
rMany p = rSome p <++ pure []

rMany1 :: forall m a. Readish m => m a -> m (NonEmpty a)
rMany1 p = liftA2 (:|) p $ rMany p

rSome :: forall m a. Readish m => m a -> m [a]
rSome p = fmap toList $ rMany1 p

rSatisfy :: Readish m => (Element (StreamableBasis m) -> Bool) -> m (Element (StreamableBasis m))
rSatisfy f = do
    a <- rItem
    if f a
        then return a
        else empty

rSkipSpaces :: ReadPrec ()
rSkipSpaces = ReadPrec.lift ReadP.skipSpaces

instance Streamable ReadPrec where
    type StreamableBasis ReadPrec = String
    rItem = ReadPrec.get
    rWhole = ReadPrec.lift $ ReadP.munch $ \_ -> True
    rLiterals s = void $ ReadPrec.lift $ ReadP.string s
    rLiteral c = void $ ReadPrec.lift $ ReadP.char c
    rExact a fa = do
        a' <- fa
        if a == a'
            then return ()
            else empty

instance Readish ReadPrec where
    (<++) = (ReadPrec.<++)

runReadPrec :: ReadPrec a -> String -> Maybe a
runReadPrec r s = let
    pickdone (a, "") = Just a
    pickdone _ = Nothing
    in case mapMaybe pickdone $ ReadPrec.readPrec_to_S r ReadPrec.minPrec s of
        [a] -> Just a
        _ -> Nothing
