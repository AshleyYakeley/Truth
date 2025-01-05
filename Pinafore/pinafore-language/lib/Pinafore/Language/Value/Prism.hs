module Pinafore.Language.Value.Prism where

import Import
import Pinafore.Language.Value.FiniteSetModel

data LangPrism (a :: (Type, Type)) (b :: (Type, Type)) =
    MkLangPrism (Contra a -> Either (Co a) (Co b))
                (Contra b -> Co a)

type LangPrism' a b = LangPrism '( a, a) '( b, b)

prism ::
       forall m a b. MonadInner m
    => (a -> m b)
    -> (b -> a)
    -> LangPrism' a b
prism amb ba =
    MkLangPrism
        (\a ->
             case mToMaybe $ amb a of
                 Just b -> Right b
                 Nothing -> Left a)
        ba

prismEncode :: forall ap aq bp bq. LangPrism '( ap, aq) '( bp, bq) -> bp -> aq
prismEncode (MkLangPrism _ e) = e

prismDecode :: forall ap aq bp bq. LangPrism '( ap, aq) '( bp, bq) -> ap -> Maybe bq
prismDecode (MkLangPrism d _) a = mToMaybe $ d a

instance CatFunctor (CatRange (->)) (->) (LangPrism a) where
    cfmap (MkCatRange pp qq) (MkLangPrism d e) = MkLangPrism (fmap qq . d) (e . pp)

instance CatFunctor (CatRange (->)) (NestedMorphism (->)) LangPrism where
    cfmap (MkCatRange pp qq) = MkNestedMorphism $ \(MkLangPrism d e) -> MkLangPrism (cfmap1 qq . d . pp) (qq . e)

instance MaybeRepresentational LangPrism where
    maybeRepresentational = Nothing

instance MaybeRepresentational (LangPrism a) where
    maybeRepresentational = Nothing

instance HasCCRVariance 'RangeCCRVariance LangPrism

instance HasCCRVariance 'RangeCCRVariance (LangPrism a)

identityLangPrism :: forall x y. LangPrism '( x, y) '( y, x)
identityLangPrism = MkLangPrism Right id

composeLangPrism ::
       forall ap aq bx by cp cq.
       LangPrism '( bx, by) '( cp, cq)
    -> LangPrism '( ap, aq) '( by, bx)
    -> LangPrism '( ap, aq) '( cp, cq)
composeLangPrism (MkLangPrism dBC eBC) (MkLangPrism dAB eAB) = let
    dAC a =
        case dAB a of
            Left a' -> Left a'
            Right b ->
                case dBC b of
                    Left b' -> Left $ eAB b'
                    Right c -> Right c
    eAC = eAB . eBC
    in MkLangPrism dAC eAC

codecToPrism ::
       forall m a b. MonadInner m
    => Codec' m a b
    -> LangPrism' a b
codecToPrism MkCodec {..} = prism decode encode

prismToCodec :: LangPrism' a b -> Codec a b
prismToCodec p = let
    decode = prismDecode p
    encode = prismEncode p
    in MkCodec {..}

langPrismApplySet ::
       forall ap aq b. LangPrism '( aq, ap) '( b, b) -> LangFiniteSetModel '( ap, aq) -> LangFiniteSetModel '( b, b)
langPrismApplySet (MkLangPrism dec enc) (MkLangFiniteSetModel eqv (MkRange pt qt :: _ t _) model) = let
    codec :: Codec t b
    codec = MkCodec (mToMaybe . dec . shimToFunction qt) (shimToFunction pt . enc)
    in MkLangFiniteSetModel (contramap (encode codec) eqv) (MkRange id id) $
       eaMap (giveConstraint eqv $ codecFiniteSetChangeLens codec) model
