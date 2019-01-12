module Language.Expression.Sealed where

import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Polarity
import Language.Expression.TypeF
import Language.Expression.TypeMappable
import Shapes

data SealedExpression name vw tw =
    forall t. MkSealedExpression (tw t)
                                 (NamedExpression name vw t)

constSealedExpression :: AnyValue tw -> SealedExpression name vw tw
constSealedExpression (MkAnyValue twt t) = MkSealedExpression twt $ pure t

evalSealedExpression :: (MonadFail m, Show name) => SealedExpression name vw tw -> m (AnyValue tw)
evalSealedExpression (MkSealedExpression twa expr) = do
    a <- evalExpression expr
    return $ MkAnyValue twa a

varSealedExpression :: name -> vw tv -> tw tt -> (tv -> tt) -> SealedExpression name vw tw
varSealedExpression n vwt twt conv = MkSealedExpression twt $ fmap conv $ varNamedExpression n vwt

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

instance TypeMappable poswit negwit (SealedExpression name negwit poswit) where
    mapTypesM mapPos mapNeg (MkSealedExpression tt expr) = do
        MkTypeF tt' conv <- mapPos tt
        expr' <- mapTypesM mapPos mapNeg expr
        return $ MkSealedExpression tt' $ fmap conv expr'

data SealedPattern name vw tw =
    forall t. MkSealedPattern (tw t)
                              (NamedPattern name vw t ())

instance TypeMappable poswit negwit (SealedPattern name poswit negwit) where
    mapTypesM mapPos mapNeg (MkSealedPattern tt pat) = do
        MkTypeF tt' conv <- mapNeg tt
        pat' <- mapTypesM mapPos mapNeg pat
        return $ MkSealedPattern tt' $ pat' . arr conv

typeFConstExpression :: TypeF poswit 'Positive t -> t -> SealedExpression name negwit poswit
typeFConstExpression (MkTypeF tt conv) t = MkSealedExpression tt $ pure $ conv t

varSealedPattern :: name -> tw t -> vw v -> (t -> v) -> SealedPattern name vw tw
varSealedPattern n twt vwt conv = MkSealedPattern twt $ varNamedPattern n vwt . arr conv

anySealedPattern :: tw t -> SealedPattern name vw tw
anySealedPattern twt = MkSealedPattern twt $ pure ()

data PatternConstructor (name :: Type) (vw :: Type -> Type) (tw :: Type -> Type) =
    forall t lt. MkPatternConstructor (tw t)
                                      (ListType vw lt)
                                      (NamedPattern name vw t (HList lt))

data HListPolWit wit t where
    MkHListPolWit :: ListType wit t -> HListPolWit wit (HList t)

liftHListPolwit ::
       forall m wit polarity. (Is PolarityType polarity, Applicative m)
    => (forall t. wit t -> m (TypeF wit polarity t))
    -> forall t'. HListPolWit wit t' -> m (TypeF (HListPolWit wit) polarity t')
liftHListPolwit _ff (MkHListPolWit NilListType) = pure $ mkTypeF $ MkHListPolWit NilListType
liftHListPolwit ff (MkHListPolWit (ConsListType t tt)) = let
    combineTFs ::
           forall a lt.
           TypeF wit polarity a
        -> TypeF (HListPolWit wit) polarity (HList lt)
        -> TypeF (HListPolWit wit) polarity (a, HList lt)
    combineTFs (MkTypeF w1 conv1) (MkTypeF (MkHListPolWit wr) convr) =
        MkTypeF (MkHListPolWit $ ConsListType w1 wr) $
        case representative @_ @_ @polarity of
            PositiveType -> \(a1, ar) -> (conv1 a1, convr ar)
            NegativeType -> \(a1, ar) -> (conv1 a1, convr ar)
    in combineTFs <$> ff t <*> liftHListPolwit ff (MkHListPolWit tt)

instance TypeMappable (poswit :: Type -> Type) (negwit :: Type -> Type) (PatternConstructor name poswit negwit) where
    mapTypesM mapPos mapNeg (MkPatternConstructor (tt :: negwit t) lvw pat) = do
        MkTypeF tt' conv <- mapNeg tt
        pat' <- mapTypesM @poswit @negwit mapPos mapNeg pat
        MkTypeF (MkHListPolWit lvw') lconv <-
            mapTypesM (liftHListPolwit mapPos) mapNeg $ mkTypeF @'Positive $ MkHListPolWit lvw
        return $ MkPatternConstructor tt' lvw' $ fmap lconv $ pat' . arr conv

sealedPatternConstructor :: MonadFail m => PatternConstructor name vw tw -> m (SealedPattern name vw tw)
sealedPatternConstructor (MkPatternConstructor twt NilListType pat) = return $ MkSealedPattern twt pat
sealedPatternConstructor _ = fail "Not enough arguments to constructor in pattern"
