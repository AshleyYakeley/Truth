module Data.ReadShow
    ( ReadShow(..)
    , readShow
    , digitsReadShow
    , negateReadShow
    , integerReadShow
    ) where

import Data.Codec
import Data.Streamable
import Shapes.Import

data ReadShow a = MkReadShow
    { rsShow :: a -> String
    , rsRead :: ReadPrec a
    }

instance Invariant ReadShow where
    invmap ab ba (MkReadShow s r) = MkReadShow (s . ba) (fmap ab r)

instance Productable ReadShow where
    rUnit :: ReadShow ()
    rUnit = let
        rsShow () = mempty
        rsRead = rUnit
        in MkReadShow {..}
    (<***>) :: forall a b. ReadShow a -> ReadShow b -> ReadShow (a, b)
    MkReadShow sa ra <***> MkReadShow sb rb = let
        sab (a, b) = sa a <> sb b
        rab = ra <***> rb
        in MkReadShow sab rab

instance Summable ReadShow where
    rVoid :: ReadShow Void
    rVoid = let
        rsShow n = never n
        rsRead = rVoid
        in MkReadShow {..}
    MkReadShow sa ra <+++> MkReadShow sb rb = let
        sab (Left a) = sa a
        sab (Right b) = sb b
        rab = ra <+++> rb
        in MkReadShow sab rab

readShow :: (Read a, Show a) => ReadShow a
readShow = MkReadShow {rsShow = show, rsRead = readPrec}

instance CodecMap ReadShow where
    codecMap MkCodec {..} (MkReadShow s r) =
        MkReadShow (s . encode) $ do
            a <- r
            mpure $ decode a

instance Riggable ReadShow where
    rOptional (MkReadShow s r) = let
        s' Nothing = mempty
        s' (Just x) = s x
        r' = rOptional r
        in MkReadShow s' r'
    rList (MkReadShow s r) = let
        s' [] = mempty
        s' (x:xs) = s x <> s' xs
        r' = rList r
        in MkReadShow s' r'
    rList1 (MkReadShow s r) = let
        s' [] = mempty
        s' (x:xs) = s'' (x :| xs)
        s'' (x :| xs) = s x <> s' xs
        r' = rList1 r
        in MkReadShow s'' r'

instance Streamable ReadShow where
    type StreamableBasis ReadShow = String
    rItem = MkReadShow pure rItem
    rWhole = MkReadShow id rWhole
    rLiterals s = let
        rsShow () = s
        rsRead = rLiterals s
        in MkReadShow {..}
    rLiteral c = let
        rsShow () = pure c
        rsRead = rLiteral c
        in MkReadShow {..}
    rExact a (MkReadShow s r) = let
        rsShow () = s a
        rsRead = rExact a r
        in MkReadShow {..}

digitCodec :: Codec Char Integer
digitCodec = let
    encode i = intToDigit $ fromInteger i
    decode c =
        if isDigit c
            then return $ toInteger $ digitToInt c
            else empty
    in MkCodec {..}

digitReadShow :: ReadShow Integer
digitReadShow = codecMap digitCodec rItem

assembleDigits :: Integer -> [Integer] -> Integer
assembleDigits i [] = i
assembleDigits i (d:dd) = assembleDigits (i * 10 + d) dd

digitsReadShow :: ReadShow Integer
digitsReadShow = let
    nonEmptyToInteger :: NonEmpty Integer -> Integer
    nonEmptyToInteger xs = assembleDigits 0 $ toList xs
    integerToList :: Integer -> [Integer]
    integerToList 0 = []
    integerToList n = integerToList (div n 10) <> [mod n 10]
    integerToNonEmpty :: Integer -> NonEmpty Integer
    integerToNonEmpty n = fromMaybe (pure 0) $ nonEmpty $ integerToList n
    in invmap nonEmptyToInteger integerToNonEmpty $ rList1 digitReadShow

negateReadShow ::
       forall a. (Ord a, Num a)
    => ReadShow a
    -> ReadShow a
negateReadShow ra = let
    tupleToInteger :: (Maybe (), a) -> a
    tupleToInteger (Nothing, i) = i
    tupleToInteger (Just (), i) = negate i
    integerToTuple :: a -> (Maybe (), a)
    integerToTuple i
        | i < 0 = (Just (), negate i)
    integerToTuple i = (Nothing, i)
    in invmap tupleToInteger integerToTuple $ rOptional (rLiteral '-') <***> ra

integerReadShow :: ReadShow Integer
integerReadShow = negateReadShow digitsReadShow
