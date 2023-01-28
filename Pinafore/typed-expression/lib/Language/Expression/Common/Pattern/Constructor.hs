{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Pattern.Constructor where

import Language.Expression.Common.Error
import Language.Expression.Common.Pattern.Pattern
import Language.Expression.Common.Pattern.Sealed
import Language.Expression.Common.WitnessTraversable
import Shapes

data PatternConstructor (patwit :: Type -> Type) (poswit :: Type -> Type) (negwit :: Type -> Type) =
    forall (t :: Type) (lt :: [Type]). MkPatternConstructor (negwit t)
                                                            (ListType poswit lt)
                                                            (Pattern patwit t (ListProduct lt))

toPatternConstructor ::
       forall patwit poswit negwit t lt.
       negwit t
    -> ListType poswit lt
    -> PurityFunction Maybe t (ListProduct lt)
    -> PatternConstructor patwit poswit negwit
toPatternConstructor nwt tlt f = MkPatternConstructor nwt tlt $ purityFunctionPattern f

liftListProductPolwit ::
       forall m wit. Applicative m
    => EndoM' m wit
    -> EndoM' m (ListProductType wit)
liftListProductPolwit ff = MkEndoM $ \(MkListProductType lwt) -> fmap MkListProductType $ mapMListType (unEndoM ff) lwt

instance IsPatternWitness poswit patwit =>
             WitnessTraversable (poswit :: Type -> Type) (negwit :: Type -> Type) (PatternConstructor patwit poswit negwit) where
    traverseWitnessesM mapPos mapNeg =
        MkEndoM $ \(MkPatternConstructor (tt :: negwit t) (lvw :: ListType poswit lt) pat) -> do
            tt' <- unEndoM mapNeg tt
            pat' <- unEndoM (traverseWitnessesM @Type @poswit @negwit mapPos mapNeg) pat
            hwit <- unEndoM (traverseWitnessesM @Type (liftListProductPolwit mapPos) mapNeg) $ MkListProductType lvw
            pure $
                case hwit of
                    MkListProductType (lvw' :: ListType poswit lt') ->
                        case injectiveListProduct @lt @lt' of
                            Refl -> MkPatternConstructor tt' lvw' pat'

sealedPatternConstructor ::
       MonadThrow ExpressionError m => PatternConstructor patwit poswit negwit -> m (SealedPattern patwit negwit)
sealedPatternConstructor (MkPatternConstructor twt NilListType pat) = return $ MkSealedPattern twt pat
sealedPatternConstructor _ = throw PatternTooFewConsArgsError
