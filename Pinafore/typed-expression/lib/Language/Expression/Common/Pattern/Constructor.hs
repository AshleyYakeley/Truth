{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Pattern.Constructor where

import Language.Expression.Common.Error
import Language.Expression.Common.Pattern.Named
import Language.Expression.Common.Pattern.Pattern
import Language.Expression.Common.Pattern.Sealed
import Language.Expression.Common.WitnessMappable
import Shapes

data PatternConstructor (name :: Type) (vw :: Type -> Type) (tw :: Type -> Type) =
    forall (t :: Type) (lt :: [Type]). MkPatternConstructor (tw t)
                                                            (ListType vw lt)
                                                            (NamedPattern name vw t (ListProduct lt))

toPatternConstructor ::
       forall name poswit negwit t lt.
       negwit t
    -> ListType poswit lt
    -> PurityFunction Maybe t (ListProduct lt)
    -> PatternConstructor name poswit negwit
toPatternConstructor nwt tlt f = MkPatternConstructor nwt tlt $ ClosedPattern f

liftListProductPolwit ::
       forall m wit. Applicative m
    => (forall t. wit t -> m (wit t))
    -> forall t'. ListProductType wit t' -> m (ListProductType wit t')
liftListProductPolwit ff (MkListProductType lwt) = fmap MkListProductType $ mapMListType ff lwt

instance WitnessMappable (poswit :: Type -> Type) (negwit :: Type -> Type) (PatternConstructor name poswit negwit) where
    mapWitnessesM mapPos mapNeg (MkPatternConstructor (tt :: negwit t) (lvw :: ListType wit lt) pat) = do
        tt' <- mapNeg tt
        pat' <- mapWitnessesM @Type @poswit @negwit mapPos mapNeg pat
        hwit <- mapWitnessesM @Type (liftListProductPolwit mapPos) mapNeg $ MkListProductType lvw
        pure $
            case hwit of
                MkListProductType (lvw' :: ListType wit lt') ->
                    case injectiveListProduct @lt @lt' of
                        Refl -> MkPatternConstructor tt' lvw' pat'

sealedPatternConstructor ::
       MonadThrow ExpressionError m => PatternConstructor name vw tw -> m (SealedPattern name vw tw)
sealedPatternConstructor (MkPatternConstructor twt NilListType pat) = return $ MkSealedPattern twt pat
sealedPatternConstructor _ = throw PatternTooFewConsArgsError
