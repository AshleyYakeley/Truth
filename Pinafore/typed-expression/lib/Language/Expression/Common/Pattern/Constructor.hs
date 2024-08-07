{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Pattern.Constructor where

import Language.Expression.Common.Error
import Language.Expression.Common.Expression
import Language.Expression.Common.Named
import Language.Expression.Common.Pattern.Pattern
import Language.Expression.Common.Pattern.Sealed
import Language.Expression.Common.WitnessMappable
import Shapes

data PatternConstructor patwit expwit funcwit poswit =
    forall (lt :: [Type]). MkPatternConstructor (ListType poswit lt)
                                                (SealedPattern patwit expwit funcwit (ListProduct lt))

toPatternConstructor ::
       forall patwit expwit funcwit poswit t lt.
       funcwit t
    -> ListType poswit lt
    -> PurityFunction Maybe (FunctionExpression expwit) t (ListProduct lt)
    -> PatternConstructor patwit expwit funcwit poswit
toPatternConstructor nwt tlt f = MkPatternConstructor tlt $ MkSealedPattern nwt $ purePattern f

liftListProductPolwit ::
       forall m wit. Applicative m
    => EndoM' m wit
    -> EndoM' m (ListProductType wit)
liftListProductPolwit ff = MkEndoM $ \(MkListProductType lwt) -> fmap MkListProductType $ mapMListType (unEndoM ff) lwt

instance ( forall t. WitnessMappable poswit negwit (patwit t)
         , forall t. WitnessMappable poswit negwit (expwit t)
         , forall t. WitnessMappable poswit negwit (funcwit t)
         ) =>
             WitnessMappable (poswit :: Type -> Type) (negwit :: Type -> Type) (PatternConstructor patwit expwit funcwit poswit) where
    mapWitnessesM mapPos mapNeg =
        MkEndoM $ \(MkPatternConstructor (lvw :: ListType wit lt) pat) -> do
            pat' <- unEndoM (mapWitnessesM @Type mapPos mapNeg) pat
            hwit <- unEndoM (mapWitnessesM @Type (liftListProductPolwit mapPos) mapNeg) $ MkListProductType lvw
            pure $
                case hwit of
                    MkListProductType (lvw' :: ListType wit lt') ->
                        case injectiveListProduct @lt @lt' of
                            Refl -> MkPatternConstructor lvw' pat'

sealedPatternConstructor ::
       MonadThrow PatternError m
    => PatternConstructor patwit expwit funcwit poswit
    -> m (SealedPattern patwit expwit funcwit ())
sealedPatternConstructor (MkPatternConstructor NilListType pat) = return pat
sealedPatternConstructor _ = throw PatternTooFewConsArgsError

type NamedPatternConstructor (name :: Type) (poswit :: Type -> Type) (negwit :: Type -> Type)
     = PatternConstructor (NameWitness name poswit) (NameWitness name negwit) negwit poswit
