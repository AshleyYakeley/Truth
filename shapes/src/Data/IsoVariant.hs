module Data.IsoVariant where

import Shapes.Import
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec

class IsoVariant f where
    isoMap :: (a -> b) -> (b -> a) -> f a -> f b
    default isoMap :: Functor f => (a -> b) -> (b -> a) -> f a -> f b
    isoMap ab _ = fmap ab

class IsoVariant' f where
    isoMap' :: (a -> b) -> (b -> a) -> f a t -> f b t

instance IsoVariant []

instance IsoVariant Maybe

instance IsoVariant Identity

instance IsoVariant ((->) a)

instance IsoVariant' (->) where
    isoMap' _ ba at b = at $ ba b

instance IsoVariant ((,) a)

instance IsoVariant' (,) where
    isoMap' ab _ (a, t) = (ab a, t)

instance IsoVariant (Either a)

instance IsoVariant' Either where
    isoMap' ab _ (Left a) = Left $ ab a
    isoMap' _ _ (Right t) = Right t

enumMap :: (IsoVariant f, Enum a) => f Int -> f a
enumMap = isoMap toEnum fromEnum

infixr 3 <***>, ***>, <***

class IsoVariant f => Productish f where
    pUnit :: f ()
    default pUnit :: Applicative f => f ()
    pUnit = pure ()
    (<***>) :: f a -> f b -> f (a, b)
    default (<***>) :: Applicative f => f a -> f b -> f (a, b)
    (<***>) = liftA2 (,)
    (***>) :: f () -> f a -> f a
    fu ***> fa = isoMap (\((), a) -> a) (\a -> ((), a)) $ fu <***> fa
    (<***) :: f a -> f () -> f a
    fa <*** fu = isoMap (\(a, ()) -> a) (\a -> (a, ())) $ fa <***> fu

pProductLeft :: Productish f => f a -> f a -> f a
pProductLeft fa fb = isoMap fst (\a -> (a, a)) $ fa <***> fb

infixr 2 <+++>

class IsoVariant f => Summish f where
    pNone :: f None
    default pNone :: Alternative f => f None
    pNone = empty
    (<+++>) :: f a -> f b -> f (Either a b)
    default (<+++>) :: Alternative f => f a -> f b -> f (Either a b)
    fa <+++> fb = (fmap Left fa) <|> (fmap Right fb)

pSumLeft :: Summish f => f a -> f a -> f a
pSumLeft fa fb = isoMap (either id id) Left $ fa <+++> fb

class (Productish f, Summish f) => Ringish f where
    pOptional :: forall a. f a -> f (Maybe a)
    pOptional fa = let
        eitherToMaybe :: Either a () -> Maybe a
        eitherToMaybe (Left a) = Just a
        eitherToMaybe (Right ()) = Nothing
        maybeToEither :: Maybe a -> Either a ()
        maybeToEither (Just a) = Left a
        maybeToEither Nothing = Right ()
        in isoMap eitherToMaybe maybeToEither $ fa <+++> pUnit
    pList1 :: f a -> f (NonEmpty a)
    pList1 fa = let
        pairToNonEmpty :: (a, [a]) -> NonEmpty a
        pairToNonEmpty (a, as) = a :| as
        nonEmptyToPair :: NonEmpty a -> (a, [a])
        nonEmptyToPair (a :| as) = (a, as)
        in isoMap pairToNonEmpty nonEmptyToPair $ fa <***> pList fa
    pList :: f a -> f [a]
    pList fa = let
        eitherToList :: Either (NonEmpty a) () -> [a]
        eitherToList (Left (a :| aa)) = a : aa
        eitherToList (Right ()) = []
        listToEither :: [a] -> Either (NonEmpty a) ()
        listToEither (a:aa) = Left $ a :| aa
        listToEither [] = Right ()
        in isoMap eitherToList listToEither $ pList1 fa <+++> pUnit

instance IsoVariant ReadPrec

instance Productish ReadPrec

instance Summish ReadPrec where
    ra <+++> rb = fmap Left ra ReadPrec.<++ fmap Right rb

instance Ringish ReadPrec where
    pOptional ra = ReadPrec.readP_to_Prec $ \prec -> ReadP.option Nothing $ fmap Just $ ReadPrec.readPrec_to_P ra prec
    pList ra = ReadPrec.readP_to_Prec $ \prec -> ReadP.many $ ReadPrec.readPrec_to_P ra prec
