module Data.Streamable where

import Data.Filterable
import Shapes.Import
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec

class (Riggable f, Monoid (StreamableBasis f)) => Streamable f where
    type StreamableBasis f :: Type
    rItem :: f (Element (StreamableBasis f))
    rWhole :: f (StreamableBasis f)
    rLiterals :: StreamableBasis f -> f ()
    rLiteral :: Element (StreamableBasis f) -> f ()
    rExact ::
           forall a. Eq a
        => a
        -> f a
        -> f ()

class (MonadPlus m, Streamable m) => Readish m where
    (<++) :: forall a. m a -> m a -> m a
    --(<++>) :: forall a. m a -> m a -> m a

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
    --(<++>) = (ReadPrec.<++>)

runReadPrec :: ReadPrec a -> String -> Maybe a
runReadPrec r s = let
    pickdone (a, "") = Just a
    pickdone _ = Nothing
    in case mapMaybe pickdone $ ReadPrec.readPrec_to_S r ReadPrec.minPrec s of
           [a] -> Just a
           _ -> Nothing
