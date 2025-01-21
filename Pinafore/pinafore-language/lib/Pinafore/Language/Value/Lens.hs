module Pinafore.Language.Value.Lens where

import Import

data LangLens (a :: (Type, Type)) (b :: (Type, Type)) = MkLangLens
    { langLensGet :: Contra a -> Co b
    , langLensPutback :: Contra a -> Contra b -> Co a
    }

instance CatFunctor (CatRange (->)) (->) (LangLens a) where
    cfmap (MkCatRange pp qq) (MkLangLens g pb) = MkLangLens (qq . g) $ \a b -> pb a $ pp b

instance CatFunctor (CatRange (->)) (NestedMorphism (->)) LangLens where
    cfmap (MkCatRange pp qq) = MkNestedMorphism $ \(MkLangLens g pb) -> MkLangLens (g . pp) $ \a b -> qq $ pb (pp a) b

instance MaybeRepresentational LangLens where
    maybeRepresentational = Nothing

instance MaybeRepresentational (LangLens a) where
    maybeRepresentational = Nothing

instance HasCCRVariance 'RangeCCRVariance LangLens

instance HasCCRVariance 'RangeCCRVariance (LangLens a)

identityLangLens :: forall x y. LangLens '(x, y) '(y, x)
identityLangLens = MkLangLens id $ \_ -> id

composeLangLens ::
    forall ap aq bx by cp cq.
    LangLens '(bx, by) '(cp, cq) ->
    LangLens '(ap, aq) '(by, bx) ->
    LangLens '(ap, aq) '(cp, cq)
composeLangLens (MkLangLens gBC pbBC) (MkLangLens gAB pbAB) =
    MkLangLens (gBC . gAB) $ \a c -> let
        b = gAB a
        b' = pbBC b c
        in pbAB a b'

pairLangLens ::
    forall a bp bq cp cq.
    LangLens '(a, a) '(bp, bq) ->
    LangLens '(a, a) '(cp, cq) ->
    LangLens '(a, a) '((bp, cp), (bq, cq))
pairLangLens (MkLangLens gAB pbAB) (MkLangLens gAC pbAC) =
    MkLangLens (\a -> (gAB a, gAC a)) $ \a (b, c) -> let
        a' = pbAB a b
        in pbAC a' c

eitherLangLens ::
    forall ap aq bp bq cp cq.
    LangLens '(ap, aq) '(cp, cq) ->
    LangLens '(bp, bq) '(cp, cq) ->
    LangLens '(Either ap bp, Either aq bq) '(cp, cq)
eitherLangLens (MkLangLens gAC pbAC) (MkLangLens gBC pbBC) =
    MkLangLens (either gAC gBC) $ \case
        Left a -> \c -> Left $ pbAC a c
        Right b -> \c -> Right $ pbBC b c
