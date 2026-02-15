{-# OPTIONS -fno-warn-orphans #-}
module Data.Streamable where

import Text.Parsec qualified as Parsec
import Text.Parsec.Pos qualified as Parsec
import Text.Parsec.Prim qualified as Parsec
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
    -- left-biased choice; if the left succeeds then the right is ignored
    (<++) :: forall a. m a -> m a -> m a

rMaybe :: forall m a. Readish m => m a -> m (Maybe a)
rMaybe p = fmap Just p <++ pure Nothing

rOption :: forall m a. Readish m => a -> m a -> m a
rOption x p = p <++ pure x

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

instance Invariant (Parsec.ParsecT s u m) where
    invmap ab _ = fmap ab

instance Productable (Parsec.ParsecT s u m)

instance Summable (Parsec.ParsecT s u m)

instance Parsec.Stream s m t => Riggable (Parsec.ParsecT s u m) where
    rOptional = Parsec.optionMaybe
    rList1 p = do
        a1 <- p
        ar <- rList p
        pure $ a1 :| ar
    rList = Parsec.many

class ParsecElement c where
    updateParsecPos :: Parsec.SourcePos -> c -> Parsec.SourcePos

instance ParsecElement Char where
    updateParsecPos = Parsec.updatePosChar

instance
    (Parsec.Stream s m (Element s), Eq (Element s), IsSequence s, Monoid s, Show s, MonoPointed s, MonoFoldable s, ParsecElement (Element s)) =>
    Streamable (Parsec.ParsecT s u m)
    where
    type StreamableBasis (Parsec.ParsecT s u m) = s
    rItem = Parsec.tokenPrim (show . singleton @s) (\pos c _ -> updateParsecPos pos c) Just
    rWhole = Parsec.getInput
    rLiteral cc = Parsec.tokenPrim (show . singleton @s) (\pos c _ -> updateParsecPos pos c)
        $ \c -> if c == cc then Just () else Nothing
    rLiterals s = void $ Parsec.tokens' (show @s . fromList) (foldl updateParsecPos) $ otoList s
    rExact a fa = do
        a' <- fa
        if a == a'
            then return ()
            else empty

instance
    (Parsec.Stream s m (Element s), Eq (Element s), IsSequence s, Monoid s, Show s, MonoPointed s, MonoFoldable s, ParsecElement (Element s)) =>
    Readish (Parsec.ParsecT s u m)
    where
    p <++ q = Parsec.try p <|> q
