module Language.Expression.Common.Pattern.Pattern
    ( Pattern(..)
    , IsPatternWitness(..)
    , purityFunctionPattern
    , contramap1Pattern
    , patternFreeWitnesses
    , anyPattern
    , varPattern
    ) where

import Language.Expression.Common.Named
import Language.Expression.Common.WitnessTraversable
import Shapes

type Pattern :: (Type -> Type) -> Type -> Type -> Type
data Pattern patwit q a =
    forall t. MkPattern [SomeFor ((->) t) patwit]
                        (PurityFunction Maybe q (t, a))

instance Functor (Pattern patwit q) where
    fmap ab (MkPattern ww pf) = MkPattern ww $ fmap (fmap ab) pf

purityFunctionPattern :: PurityFunction Maybe a b -> Pattern patwit a b
purityFunctionPattern pf = MkPattern [] $ fmap (\b -> ((), b)) pf

contramap1Pattern :: PurityFunction Maybe q p -> Pattern patwit p a -> Pattern patwit q a
contramap1Pattern qp (MkPattern ww pf) = MkPattern ww $ pf . qp

contramapWits :: (p -> q) -> [SomeFor ((->) q) patwit] -> [SomeFor ((->) p) patwit]
contramapWits pq = fmap $ \(MkSomeFor patwit conv) -> MkSomeFor patwit $ conv . pq

combineWits :: [SomeFor ((->) p) patwit] -> [SomeFor ((->) q) patwit] -> [SomeFor ((->) (p, q)) patwit]
combineWits ww1 ww2 = contramapWits fst ww1 <> contramapWits snd ww2

instance Applicative (Pattern patwit q) where
    pure a = MkPattern [] $ pure ((), a)
    MkPattern ww1 pf <*> MkPattern ww2 qf =
        MkPattern (combineWits ww1 ww2) $ liftA2 (\(t1, ab) (t2, a) -> ((t1, t2), ab a)) pf qf

instance Category (Pattern patwit) where
    id = MkPattern [] $ arr $ \a -> ((), a)
    MkPattern ww1 bc . MkPattern ww2 ab =
        MkPattern (combineWits ww1 ww2) $ fmap (\(t2, (t1, c)) -> ((t1, t2), c)) (second bc) . ab

instance Arrow (Pattern patwit) where
    arr ab = MkPattern [] $ arr $ \a -> ((), ab a)
    first (MkPattern ww pf) = MkPattern ww $ fmap (\((t, c), d) -> (t, (c, d))) $ first pf

patternFreeWitnesses :: (forall t. patwit t -> r) -> Pattern patwit q a -> [r]
patternFreeWitnesses wr (MkPattern ww _) = fmap (\(MkSomeFor wt _) -> wr wt) ww

anyPattern :: Pattern patwit q ()
anyPattern = pure ()

varPattern :: patwit t -> Pattern patwit t ()
varPattern wt = MkPattern [MkSomeFor wt id] $ arr $ \t -> (t, ())

class IsPatternWitness (poswit :: Type -> Type) (patwit :: Type -> Type) where
    traversePatternWitness ::
           forall m. Applicative m
        => EndoM' m poswit
        -> EndoM' m patwit

instance IsPatternWitness poswit poswit where
    traversePatternWitness ew = ew

instance IsPatternWitness poswit (NameWitness name poswit) where
    traversePatternWitness (MkEndoM f) = MkEndoM $ \(MkNameWitness n w) -> fmap (MkNameWitness n) $ f w

instance IsPatternWitness poswit patwit => WitnessTraversable poswit negwit (Pattern patwit a b) where
    traverseWitnessesM mapPos _ =
        MkEndoM $ \(MkPattern ww pf) ->
            fmap (\ww' -> MkPattern ww' pf) $ unEndoM (endoFor $ endoSomeFor $ traversePatternWitness mapPos) ww
