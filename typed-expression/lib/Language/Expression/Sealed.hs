module Language.Expression.Sealed where

import Language.Expression.Error
import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Pattern
import Language.Expression.WitnessMappable
import Shapes

data SealedExpression (name :: Type) (vw :: Type -> Type) (tw :: Type -> Type) =
    forall t. MkSealedExpression (tw t)
                                 (NamedExpression name vw t)

constSealedExpression :: AnyValue tw -> SealedExpression name vw tw
constSealedExpression (MkAnyValue twt t) = MkSealedExpression twt $ pure t

evalSealedExpression :: (MonadThrow ExpressionError m, Show name) => SealedExpression name vw tw -> m (AnyValue tw)
evalSealedExpression (MkSealedExpression twa expr) = do
    a <- evalExpression expr
    return $ MkAnyValue twa a

varSealedExpression :: name -> vw t -> tw t -> SealedExpression name vw tw
varSealedExpression n vwt twt = MkSealedExpression twt $ varNamedExpression n vwt

sealedExpressionFreeNames :: SealedExpression name vw tw -> [name]
sealedExpressionFreeNames (MkSealedExpression _ expr) = namedExpressionFreeNames expr

type instance Element (SealedExpression name vw ((:~:) val)) = val

instance MonoFunctor (SealedExpression name vw ((:~:) val)) where
    omap ab (MkSealedExpression Refl expr) = MkSealedExpression Refl $ fmap ab expr

instance MonoPointed (SealedExpression name vw ((:~:) val)) where
    opoint p = constSealedExpression $ MkAnyValue Refl p

instance MonoApplicative (SealedExpression name vw ((:~:) val)) where
    oliftA2 appf (MkSealedExpression Refl vexpr) (MkSealedExpression Refl bexpr) =
        MkSealedExpression Refl $ appf <$> vexpr <*> bexpr
    osequenceA conv exprs =
        MkSealedExpression Refl $ fmap conv $ sequenceA $ fmap (\(MkSealedExpression Refl expr) -> expr) exprs

instance WitnessMappable poswit negwit (SealedExpression name negwit poswit) where
    mapWitnessesM mapPos mapNeg (MkSealedExpression tt expr) = do
        tt' <- mapPos tt
        expr' <- mapWitnessesM mapPos mapNeg expr
        return $ MkSealedExpression tt' expr'

data SealedPattern (name :: Type) (vw :: Type -> Type) (tw :: Type -> Type) =
    forall t. MkSealedPattern (tw t)
                              (NamedPattern name vw t ())

instance WitnessMappable poswit negwit (SealedPattern name poswit negwit) where
    mapWitnessesM mapPos mapNeg (MkSealedPattern tt pat) = do
        tt' <- mapNeg tt
        pat' <- mapWitnessesM mapPos mapNeg pat
        return $ MkSealedPattern tt' $ pat'

typeFConstExpression :: poswit t -> t -> SealedExpression name negwit poswit
typeFConstExpression tt t = MkSealedExpression tt $ pure t

varSealedPattern :: name -> tw t -> vw t -> SealedPattern name vw tw
varSealedPattern n twt vwt = MkSealedPattern twt $ varNamedPattern n vwt

anySealedPattern :: tw t -> SealedPattern name vw tw
anySealedPattern twt = MkSealedPattern twt $ pure ()

data PatternConstructor (name :: Type) (vw :: Type -> Type) (tw :: Type -> Type) =
    forall (t :: Type) (lt :: [Type]). MkPatternConstructor (tw t)
                                                            (ListType vw lt)
                                                            (NamedPattern name vw t (HList lt))

toPatternConstructor ::
       forall name poswit negwit t lt.
       negwit t
    -> ListType poswit lt
    -> (t -> Maybe (HList lt))
    -> PatternConstructor name poswit negwit
toPatternConstructor nwt tlt f = MkPatternConstructor nwt tlt $ ClosedPattern f

liftHListPolwit ::
       forall m wit. Applicative m
    => (forall t. wit t -> m (wit t))
    -> forall t'. HListWit wit t' -> m (HListWit wit t')
liftHListPolwit ff (MkHListWit lwt) = fmap MkHListWit $ mapMListType ff lwt

instance WitnessMappable (poswit :: Type -> Type) (negwit :: Type -> Type) (PatternConstructor name poswit negwit) where
    mapWitnessesM mapPos mapNeg (MkPatternConstructor (tt :: negwit t) (lvw :: ListType wit lt) pat) = do
        tt' <- mapNeg tt
        pat' <- mapWitnessesM @Type @poswit @negwit mapPos mapNeg pat
        MkHListWit (lvw' :: ListType wit lt') <- mapWitnessesM @Type (liftHListPolwit mapPos) mapNeg $ MkHListWit lvw
        Refl <- return $ injectiveHList @lt @lt'
        return $ MkPatternConstructor tt' lvw' pat'

sealedPatternConstructor ::
       MonadThrow ExpressionError m => PatternConstructor name vw tw -> m (SealedPattern name vw tw)
sealedPatternConstructor (MkPatternConstructor twt NilListType pat) = return $ MkSealedPattern twt pat
sealedPatternConstructor _ = throw PatternTooFewConsArgsError
