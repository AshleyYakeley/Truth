module Truth.Core.UI.Lens where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier

data UILens edit where
    MkUILens
        :: forall edita editb. Edit editb
        => EditLens edita editb
        -> UISpec editb
        -> UILens edita

instance Show (UILens edit) where
    show (MkUILens _ uispec) = "lens " ++ show uispec

instance UIType UILens where
    uiWitness = $(iowitness [t|UILens|])

ioMapAspectSpec :: (UISpec edita -> IO (UISpec editb)) -> Aspect edita -> Aspect editb
ioMapAspectSpec ff getuispec = do
    muispec <- getuispec
    case muispec of
        Just (name, uispec) -> do
            uispec' <- ff uispec
            return $ Just (name, uispec')
        Nothing -> return Nothing

mapAspectSpec :: (UISpec edita -> UISpec editb) -> Aspect edita -> Aspect editb
mapAspectSpec ff = ioMapAspectSpec (return . ff)

uiLens ::
       forall edita editb. Edit editb
    => EditLens edita editb
    -> UISpec editb
    -> UISpec edita
uiLens lens spec = MkUISpec $ MkUILens lens spec

uiConvert ::
       forall edita editb. (EditSubject edita ~ EditSubject editb, FullEdit edita, FullEdit editb)
    => UISpec editb
    -> UISpec edita
uiConvert = uiLens convertEditLens

mapAspect :: Edit editb => EditLens edita editb -> Aspect editb -> Aspect edita
mapAspect lens = mapAspectSpec $ uiLens lens

ioMapAspect :: Edit editb => IO (EditLens edita editb) -> Aspect editb -> Aspect edita
ioMapAspect mlens =
    ioMapAspectSpec $ \uispec -> do
        lens <- mlens
        return $ uiLens lens uispec

tupleEditUISpecs ::
       (TupleWitness FullEdit sel, FiniteTupleSelector sel)
    => (forall edit. FullEdit edit =>
                         sel edit -> UISpec edit)
    -> [UISpec (TupleEdit sel)]
tupleEditUISpecs getSpec =
    fmap
        (\(MkAnyWitness seledit) ->
             case tupleWitness (Proxy :: Proxy FullEdit) seledit of
                 Dict ->
                     case getSpec seledit of
                         spec -> MkUISpec $ MkUILens (tupleEditLens seledit) spec)
        tupleAllSelectors
    -- not really a bijection

maybeNothingValueBijection :: Eq a => a -> Bijection (Maybe a) a
maybeNothingValueBijection def = let
    biForwards (Just a) = a
    biForwards Nothing = def
    biBackwards a
        | a == def = Nothing
    biBackwards a = Just a
    in MkBijection {..}

maybeNothingGeneralLens :: Eq a => a -> EditLens (WholeEdit (Maybe a)) (WholeEdit a)
maybeNothingGeneralLens def = toEditLens $ maybeNothingValueBijection def

uiNothingValue :: Eq a => a -> UISpec (WholeEdit a) -> UISpec (WholeEdit (Maybe a))
uiNothingValue def = uiLens $ maybeNothingGeneralLens def
