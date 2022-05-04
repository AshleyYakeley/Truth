module Data.IsoVariant where

import Data.KindMorphism
import Shapes.Import
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec

type IsoVariant :: forall kp kq. (kp -> kq) -> Constraint
class IsoVariant (f :: kp -> kq) where
    isoMap :: forall (a :: kp) (b :: kp). KindFunction a b -> KindFunction b a -> KindFunction (f a) (f b)

instance IsoVariant [] where
    isoMap ab _ = fmap ab

instance IsoVariant Maybe where
    isoMap ab _ = fmap ab

instance IsoVariant Identity where
    isoMap ab _ = fmap ab

instance IsoVariant ((->) a) where
    isoMap ab _ = fmap ab

instance IsoVariant (->) where
    isoMap _ ba = MkNestedMorphism $ \at b -> at $ ba b

instance IsoVariant ((,) a) where
    isoMap ab _ = fmap ab

instance IsoVariant (,) where
    isoMap ab _ = MkNestedMorphism $ \(a, t) -> (ab a, t)

instance IsoVariant (Either a) where
    isoMap ab _ = fmap ab

instance IsoVariant Either where
    isoMap ab _ =
        MkNestedMorphism $ \case
            Left a -> Left $ ab a
            Right t -> Right t

enumMap :: (IsoVariant f, Enum a) => f Int -> f a
enumMap = isoMap toEnum fromEnum

infixr 3 <***>, ***>, <***

type Productish :: (Type -> Type) -> Constraint
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

type Summish :: (Type -> Type) -> Constraint
class IsoVariant f => Summish f where
    pNone :: f Void
    default pNone :: Alternative f => f Void
    pNone = empty
    (<+++>) :: f a -> f b -> f (Either a b)
    default (<+++>) :: Alternative f => f a -> f b -> f (Either a b)
    fa <+++> fb = (fmap Left fa) <|> (fmap Right fb)

pSumLeft :: Summish f => f a -> f a -> f a
pSumLeft fa fb = isoMap (either id id) Left $ fa <+++> fb

type Ringish :: (Type -> Type) -> Constraint
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

instance IsoVariant Endo where
    isoMap ab ba (Endo f) = Endo $ ab . f . ba

instance Productish Endo where
    pUnit = Endo id
    Endo p <***> Endo q = Endo $ \(a, b) -> (p a, q b)

instance Summish Endo where
    pNone = Endo id
    Endo p <+++> Endo q =
        Endo $ \case
            Left a -> Left $ p a
            Right b -> Right $ q b

instance Ringish Endo where
    pOptional (Endo f) = Endo $ fmap f
    pList1 (Endo f) = Endo $ fmap f
    pList (Endo f) = Endo $ fmap f

instance IsoVariant m => IsoVariant (Kleisli m a) where
    isoMap ab ba (Kleisli f) = Kleisli $ \a -> isoMap ab ba $ f a

instance Productish m => Productish (Kleisli m a) where
    pUnit = Kleisli $ \_ -> pUnit
    Kleisli p <***> Kleisli q = Kleisli $ \a -> p a <***> q a

instance Summish m => Summish (Kleisli m a) where
    pNone = Kleisli $ \_ -> pNone
    Kleisli p <+++> Kleisli q = Kleisli $ \a -> p a <+++> q a

instance Ringish m => Ringish (Kleisli m a) where
    pOptional (Kleisli f) = Kleisli $ \a -> pOptional $ f a
    pList1 (Kleisli f) = Kleisli $ \a -> pList1 $ f a
    pList (Kleisli f) = Kleisli $ \a -> pList $ f a

instance IsoVariant ReadPrec where
    isoMap ab _ = fmap ab

instance Productish ReadPrec

instance Summish ReadPrec where
    ra <+++> rb = fmap Left ra ReadPrec.<++ fmap Right rb

instance Ringish ReadPrec where
    pOptional ra = ReadPrec.readP_to_Prec $ \prec -> ReadP.option Nothing $ fmap Just $ ReadPrec.readPrec_to_P ra prec
    pList ra = ReadPrec.readP_to_Prec $ \prec -> ReadP.many $ ReadPrec.readPrec_to_P ra prec
