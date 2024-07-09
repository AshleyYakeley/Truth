{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Pattern.Constructor where

import Language.Expression.Common.Error
import Language.Expression.Common.Pattern.Pattern
import Language.Expression.Common.Pattern.Sealed
import Language.Expression.Common.WitnessMappable
import Shapes

data PatternConstructor patvar poswit exprw =
    forall (t :: Type) (lt :: [Type]). MkPatternConstructor (exprw t)
                                                            (ListType poswit lt)
                                                            (Pattern patvar t (ListProduct lt))

toPatternConstructor ::
       forall patvar poswit exprw t lt.
       exprw t
    -> ListType poswit lt
    -> PurityFunction Maybe t (ListProduct lt)
    -> PatternConstructor patvar poswit exprw
toPatternConstructor nwt tlt f = MkPatternConstructor nwt tlt $ purityFunctionPattern f

liftListProductPolwit ::
       forall m wit. Applicative m
    => EndoM' m wit
    -> EndoM' m (ListProductType wit)
liftListProductPolwit ff = MkEndoM $ \(MkListProductType lwt) -> fmap MkListProductType $ mapMListType (unEndoM ff) lwt

instance (forall t. WitnessMappable poswit negwit (patvar t), forall t. WitnessMappable poswit negwit (exprw t)) =>
             WitnessMappable (poswit :: Type -> Type) (negwit :: Type -> Type) (PatternConstructor patvar poswit exprw) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \(MkPatternConstructor (tt :: exprw t) (lvw :: ListType wit lt) pat) -> do
            tt' <- unEndoM (mapWitnessesM @Type mapPos mapNeg) tt
            pat' <- unEndoM (mapWitnessesM @Type mapPos mapNeg) pat
            hwit <- unEndoM (mapWitnessesM @Type (liftListProductPolwit mapPos) mapNeg) $ MkListProductType lvw
            pure $
                case hwit of
                    MkListProductType (lvw' :: ListType wit lt') ->
                        case injectiveListProduct @lt @lt' of
                            Refl -> MkPatternConstructor tt' lvw' pat'

sealedPatternConstructor ::
       MonadThrow PatternError m => PatternConstructor patvar poswit exprw -> m (SealedPattern patvar exprw)
sealedPatternConstructor (MkPatternConstructor twt NilListType pat) = return $ MkSealedPattern twt pat
sealedPatternConstructor _ = throw PatternTooFewConsArgsError
