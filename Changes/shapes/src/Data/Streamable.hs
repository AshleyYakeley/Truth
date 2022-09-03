module Data.Streamable where

import Data.Filterable
import Shapes.Import
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec

class (Riggable f, Monoid (StreamableBasis f)) => Streamable f where
    type StreamableBasis f :: Type
    pItem :: f (Element (StreamableBasis f))
    pWhole :: f (StreamableBasis f)
    pLiterals :: StreamableBasis f -> f ()
    pLiteral :: Element (StreamableBasis f) -> f ()
    pExact ::
           forall a. Eq a
        => a
        -> f a
        -> f ()

class (MonadPlus m, Streamable m) => Readish m where
    (<++) :: forall a. m a -> m a -> m a
    --(<++>) :: forall a. m a -> m a -> m a

pSatisfy :: Readish m => (Element (StreamableBasis m) -> Bool) -> m (Element (StreamableBasis m))
pSatisfy f = do
    a <- pItem
    if f a
        then return a
        else empty

pSkipSpaces :: ReadPrec ()
pSkipSpaces = ReadPrec.lift ReadP.skipSpaces

instance Streamable ReadPrec where
    type StreamableBasis ReadPrec = String
    pItem = ReadPrec.get
    pWhole = ReadPrec.lift $ ReadP.munch $ \_ -> True
    pLiterals s = void $ ReadPrec.lift $ ReadP.string s
    pLiteral c = void $ ReadPrec.lift $ ReadP.char c
    pExact a fa = do
        a' <- fa
        if a == a'
            then return ()
            else empty

instance Readish ReadPrec where
    (<++) = (ReadPrec.<++)
    --(<++>) = (ReadPrec.<++>)

runReadPrec :: ReadPrec a -> String -> Maybe a
runReadPrec r s = let
    pickdone (a, "") = Just a
    pickdone _ = Nothing
    in case mapMaybe pickdone $ ReadPrec.readPrec_to_S r ReadPrec.minPrec s of
           [a] -> Just a
           _ -> Nothing
