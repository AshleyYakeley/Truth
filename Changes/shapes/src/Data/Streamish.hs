module Data.Streamish where

import Data.Filterable
import Shapes.Import
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec

class (Riggish f, Monoid (StreamishBasis f)) => Streamish f where
    type StreamishBasis f :: Type
    pItem :: f (Element (StreamishBasis f))
    pWhole :: f (StreamishBasis f)
    pLiterals :: StreamishBasis f -> f ()
    pLiteral :: Element (StreamishBasis f) -> f ()
    pExact ::
           forall a. Eq a
        => a
        -> f a
        -> f ()

class (MonadPlus m, Streamish m) => Readish m where
    (<++) :: forall a. m a -> m a -> m a
    --(<++>) :: forall a. m a -> m a -> m a

pSatisfy :: Readish m => (Element (StreamishBasis m) -> Bool) -> m (Element (StreamishBasis m))
pSatisfy f = do
    a <- pItem
    if f a
        then return a
        else empty

pSkipSpaces :: ReadPrec ()
pSkipSpaces = ReadPrec.lift ReadP.skipSpaces

instance Streamish ReadPrec where
    type StreamishBasis ReadPrec = String
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
