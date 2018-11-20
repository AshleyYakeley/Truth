module Pinafore.Base.ConstructorAdapter where

import Pinafore.Base.Know
import Pinafore.Base.Morphism
import Pinafore.Base.Point
import Shapes
import Truth.Core

data AConstructorAdapter baseedit t a b = MkAConstructorAdapter
    { caConvert :: b -> a
    , caGet :: forall m. MonadIO m => a -> MutableRead m (EditReader baseedit) -> t m (Know b)
    , caPut :: forall m. MonadIO m => b -> MutableRead m (EditReader baseedit) -> t m [baseedit]
    }

type ConstructorAdapter baseedit = CloseUnlift (AConstructorAdapter baseedit)

constructorAdapterConvert :: ConstructorAdapter baseedit a b -> b -> a
constructorAdapterConvert (MkCloseUnlift _ aca) = caConvert aca

bijectionConstructorAdapter :: forall baseedit a b. Bijection a b -> ConstructorAdapter baseedit a b
bijectionConstructorAdapter (MkBijection ab ba) =
    MkCloseUnlift identityUnlift $ let
        caConvert = ba
        caGet ::
               forall m. MonadIO m
            => a
            -> MutableRead m (EditReader baseedit)
            -> IdentityT m (Know b)
        caGet a _ = return $ Known $ ab a
        caPut _ _ = return []
        in MkAConstructorAdapter {..}

mapConstructorAdapter ::
       forall baseedit1 baseedit2 a b.
       EditLens baseedit1 baseedit2
    -> ConstructorAdapter baseedit2 a b
    -> ConstructorAdapter baseedit1 a b
mapConstructorAdapter lens ca = let
    alc :: forall t1 t2. (MonadTransUnlift t1, MonadTransUnlift t2)
        => AConstructorAdapter baseedit2 t1 a b
        -> AnEditLens t2 baseedit1 baseedit2
        -> AConstructorAdapter baseedit1 (ComposeT t1 t2) a b
    alc (MkAConstructorAdapter cconv cget cput) (MkAnEditLens (MkAnEditFunction baseGet _baseUpdate) basePE) = let
        cget' ::
               forall m. MonadIO m
            => a
            -> MutableRead m (EditReader baseedit1)
            -> ComposeT t1 t2 m (Know b)
        cget' a mr1 = MkComposeT $ withTransConstraintTM' @MonadIO $ cget a (baseGet mr1)
        cput' ::
               forall m. MonadIO m
            => b
            -> MutableRead m (EditReader baseedit1)
            -> ComposeT t1 t2 m [baseedit1]
        cput' b mr1 =
            withTransConstraintTM @MonadIO $ do
                be2 <- MkComposeT $ withTransConstraintTM' @MonadIO $ cput b (baseGet mr1)
                mbe1 <- lift2ComposeT' $ withTransConstraintTM @MonadIO $ basePE be2 mr1
                return $ fromMaybe [] mbe1
        in MkAConstructorAdapter cconv cget' cput'
    in joinUnlifts alc ca lens

constructorAdapterForwardMorphism ::
       forall baseedit a b. ConstructorAdapter baseedit a b -> PinaforeLensMorphism baseedit a b
constructorAdapterForwardMorphism (MkCloseUnlift (unlift :: Unlift tr) MkAConstructorAdapter {..}) = let
    pmForward :: AnEditLens tr (ContextEdit baseedit (WholeEdit (Know a))) (WholeEdit (Know b))
    pmForward = let
        efGet ::
               forall m. MonadIO m
            => MutableRead m (ContextEditReader baseedit (WholeEdit (Know a)))
            -> MutableRead (tr m) (WholeReader (Know b))
        efGet mr ReadWhole =
            withTransConstraintTM @Monad $ do
                ka <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
                case ka of
                    Unknown -> return Unknown
                    Known a -> caGet a $ tupleReadFunction SelectContext mr
        efUpdate ::
               forall m. MonadIO m
            => ContextEdit baseedit (WholeEdit (Know a))
            -> MutableRead m (ContextEditReader baseedit (WholeEdit (Know a)))
            -> tr m [WholeEdit (Know b)]
        efUpdate _ _ = lift $ return []
        elFunction :: AnEditFunction tr (ContextEdit baseedit (WholeEdit (Know a))) (WholeEdit (Know b))
        elFunction = MkAnEditFunction {..}
        elPutEdit ::
               forall m. MonadIO m
            => WholeEdit (Know b)
            -> MutableRead m (ContextEditReader baseedit (WholeEdit (Know a)))
            -> tr m (Maybe [ContextEdit baseedit (WholeEdit (Know a))])
        elPutEdit (MkWholeEdit Unknown) _mr =
            lift $ return $ Just $ pure $ MkTupleEdit SelectContent $ MkWholeEdit Unknown
        elPutEdit (MkWholeEdit (Known b)) mr =
            withTransConstraintTM @Monad $ do
                edits <- caPut b $ tupleReadFunction SelectContext mr
                return $
                    Just $
                    MkTupleEdit SelectContent (MkWholeEdit $ Known $ caConvert b) :
                    fmap (MkTupleEdit SelectContext) edits
        elPutEdits ::
               forall m. MonadIO m
            => [WholeEdit (Know b)]
            -> MutableRead m (ContextEditReader baseedit (WholeEdit (Know a)))
            -> tr m (Maybe [ContextEdit baseedit (WholeEdit (Know a))])
        elPutEdits [] _ = lift $ return $ Just []
        elPutEdits [edit] mr = elPutEdit edit mr
        elPutEdits (_:edits) mr = elPutEdits edits mr -- just use the last WholeEdit.
        in MkAnEditLens {..}
    pmInverse :: APinaforeFunctionMorphism baseedit tr b (FiniteSet a)
    pmInverse = let
        pfFuncRead ::
               forall m. MonadIO m
            => MutableRead m (EditReader baseedit)
            -> b
            -> tr m (FiniteSet a)
        pfFuncRead _ b = lift $ return $ opoint $ caConvert b
        pfUpdate ::
               forall m. MonadIO m
            => baseedit
            -> MutableRead m (EditReader baseedit)
            -> tr m Bool
        pfUpdate _ _ = lift $ return False
        in MkAPinaforeFunctionMorphism {..}
    in MkCloseUnlift unlift MkAPinaforeLensMorphism {..}

constructorAdapterBackwardMorphism ::
       forall baseedit a b. ConstructorAdapter baseedit a b -> PinaforeLensMorphism baseedit b a
constructorAdapterBackwardMorphism (MkCloseUnlift (unlift :: Unlift tr) MkAConstructorAdapter {..}) = let
    pmForward :: AnEditLens tr (ContextEdit baseedit (WholeEdit (Know b))) (WholeEdit (Know a))
    pmForward = let
        efGet ::
               forall m. MonadIO m
            => MutableRead m (ContextEditReader baseedit (WholeEdit (Know b)))
            -> MutableRead (tr m) (WholeReader (Know a))
        efGet mr ReadWhole =
            withTransConstraintTM @Monad $ do
                kb <- lift $ mr $ MkTupleEditReader SelectContent ReadWhole
                return $ fmap caConvert kb
        efUpdate ::
               forall m. MonadIO m
            => ContextEdit baseedit (WholeEdit (Know b))
            -> MutableRead m (ContextEditReader baseedit (WholeEdit (Know b)))
            -> tr m [WholeEdit (Know a)]
        efUpdate _ _ = lift $ return []
        elFunction :: AnEditFunction tr (ContextEdit baseedit (WholeEdit (Know b))) (WholeEdit (Know a))
        elFunction = MkAnEditFunction {..}
        elPutEdit ::
               forall m. MonadIO m
            => WholeEdit (Know a)
            -> MutableRead m (ContextEditReader baseedit (WholeEdit (Know b)))
            -> tr m (Maybe [ContextEdit baseedit (WholeEdit (Know b))])
        elPutEdit (MkWholeEdit Unknown) _mr =
            lift $ return $ Just $ pure $ MkTupleEdit SelectContent $ MkWholeEdit Unknown
        elPutEdit (MkWholeEdit (Known a)) mr =
            withTransConstraintTM @Monad $ do
                kb <- caGet a $ tupleReadFunction SelectContext mr
                return $ Just $ pure $ MkTupleEdit SelectContent (MkWholeEdit kb)
        elPutEdits ::
               forall m. MonadIO m
            => [WholeEdit (Know a)]
            -> MutableRead m (ContextEditReader baseedit (WholeEdit (Know b)))
            -> tr m (Maybe [ContextEdit baseedit (WholeEdit (Know b))])
        elPutEdits [] _ = lift $ return $ Just []
        elPutEdits [edit] mr = elPutEdit edit mr
        elPutEdits (_:edits) mr = elPutEdits edits mr -- just use the last WholeEdit.
        in MkAnEditLens {..}
    pmInverse :: APinaforeFunctionMorphism baseedit tr a (FiniteSet b)
    pmInverse = let
        pfFuncRead ::
               forall m. MonadIO m
            => MutableRead m (EditReader baseedit)
            -> a
            -> tr m (FiniteSet b)
        pfFuncRead mr a =
            withTransConstraintTM @Monad $ do
                kb <- caGet a mr
                return $
                    case kb of
                        Unknown -> mempty
                        Known b -> opoint b
        pfUpdate ::
               forall m. MonadIO m
            => baseedit
            -> MutableRead m (EditReader baseedit)
            -> tr m Bool
        pfUpdate _ _ = lift $ return False
        in MkAPinaforeFunctionMorphism {..}
    in MkCloseUnlift unlift MkAPinaforeLensMorphism {..}

type PointAdapter baseedit = ConstructorAdapter baseedit Point
