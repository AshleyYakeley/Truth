module Data.ReadShow
    ( ReadShow(..)
    , readShow
    , digitsReadShow
    , negateReadShow
    , integerReadShow
    ) where

import Data.Codec
import Data.Streamish
import Shapes.Import

data ReadShow a = MkReadShow
    { rsShow :: a -> String
    , rsRead :: ReadPrec a
    }

instance IsoVariant ReadShow where
    isoMap ab ba (MkReadShow s r) = MkReadShow (s . ba) (fmap ab r)

instance Productish ReadShow where
    pUnit :: ReadShow ()
    pUnit = let
        rsShow () = mempty
        rsRead = pUnit
        in MkReadShow {..}
    (<***>) :: forall a b. ReadShow a -> ReadShow b -> ReadShow (a, b)
    MkReadShow sa ra <***> MkReadShow sb rb = let
        sab (a, b) = sa a <> sb b
        rab = ra <***> rb
        in MkReadShow sab rab

instance Summish ReadShow where
    pNone :: ReadShow Void
    pNone = let
        rsShow n = never n
        rsRead = pNone
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

instance Ringish ReadShow where
    pOptional (MkReadShow s r) = let
        s' Nothing = mempty
        s' (Just x) = s x
        r' = pOptional r
        in MkReadShow s' r'
    pList (MkReadShow s r) = let
        s' [] = mempty
        s' (x:xs) = s x <> s' xs
        r' = pList r
        in MkReadShow s' r'
    pList1 (MkReadShow s r) = let
        s' [] = mempty
        s' (x:xs) = s'' (x :| xs)
        s'' (x :| xs) = s x <> s' xs
        r' = pList1 r
        in MkReadShow s'' r'

instance Streamish ReadShow where
    type StreamishBasis ReadShow = String
    pItem = MkReadShow pure pItem
    pLiterals s = let
        rsShow () = s
        rsRead = pLiterals s
        in MkReadShow {..}
    pLiteral c = let
        rsShow () = pure c
        rsRead = pLiteral c
        in MkReadShow {..}
    pExact a (MkReadShow s r) = let
        rsShow () = s a
        rsRead = pExact a r
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
digitReadShow = codecMap digitCodec pItem

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
    in isoMap nonEmptyToInteger integerToNonEmpty $ pList1 digitReadShow

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
    in isoMap tupleToInteger integerToTuple $ pOptional (pLiteral '-') <***> ra

integerReadShow :: ReadShow Integer
integerReadShow = negateReadShow digitsReadShow
