{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Common.Pattern.Constructor where

import Shapes

import Language.Expression.Common.Open
import Language.Expression.Common.Pattern.Pattern
import Language.Expression.Common.Pattern.Sealed

data PatternConstructor patwit expwit funcwit poswit
    = forall (lt :: [Type]). MkPatternConstructor
        (ListType poswit lt)
        (SealedPattern patwit expwit funcwit (ListProduct lt))

toPatternConstructor ::
    forall patwit expwit funcwit poswit t lt.
    funcwit t ->
    ListType poswit lt ->
    PurityFunction Maybe (Expression expwit) t (ListProduct lt) ->
    PatternConstructor patwit expwit funcwit poswit
toPatternConstructor nwt tlt f = MkPatternConstructor tlt $ MkSealedPattern nwt $ purePattern f

liftListProductPolwit ::
    forall m wit.
    Applicative m =>
    EndoM' m wit ->
    EndoM' m (ListProductType wit)
liftListProductPolwit ff = MkEndoM $ \(MkListProductType lwt) -> fmap MkListProductType $ mapMListType (unEndoM ff) lwt

sealedPatternConstructor ::
    MonadThrow PatternError m =>
    PatternConstructor patwit expwit funcwit poswit ->
    m (SealedPattern patwit expwit funcwit ())
sealedPatternConstructor (MkPatternConstructor NilListType pat) = return pat
sealedPatternConstructor _ = throw PatternTooFewConsArgsError

type NamedPatternConstructor (name :: Type) (poswit :: Type -> Type) (negwit :: Type -> Type) =
    PatternConstructor (NameWitness name poswit) (NameWitness name negwit) negwit poswit
