module Truth.Core.UI.Specifier.One where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object
import Truth.Core.Types
import Truth.Core.UI.Specifier.Selection
import Truth.Core.UI.Specifier.Specifier

data OneUISpec where
    -- view can create object
    OneWholeUISpec
        :: forall f update. (IsUpdate update, MonadOne f, FullEdit (UpdateEdit update))
        => Subscriber (FullResultOneUpdate f update)
        -> (f (Subscriber update) -> CVUISpec)
        -> SelectNotify (f ())
        -> OneUISpec

instance Show OneUISpec where
    show (OneWholeUISpec _ _ _) = "one+whole"

instance UIType OneUISpec where
    uiWitness = $(iowitness [t|OneUISpec|])

oneWholeUISpec ::
       forall f update. (IsUpdate update, MonadOne f, FullEdit (UpdateEdit update))
    => Subscriber (FullResultOneUpdate f update)
    -> (f (Subscriber update) -> CVUISpec)
    -> CVUISpec
oneWholeUISpec sub spec = mkCVUISpec $ OneWholeUISpec sub spec mempty

oneWholeSelUISpec ::
       forall sel f update. (IsUpdate update, MonadOne f, FullEdit (UpdateEdit update))
    => Subscriber (FullResultOneUpdate f update)
    -> (f (Subscriber update, SelectNotify sel) -> CVUISpec)
    -> SelectNotify (f sel)
    -> CVUISpec
oneWholeSelUISpec subf specsel snfsel = let
    spec :: f (Subscriber update) -> CVUISpec
    spec fsub = specsel $ fmap (\sub -> (sub, contramap pure snfsel)) fsub
    getf :: f () -> Maybe (f sel)
    getf fu =
        case retrieveOne fu of
            SuccessResult _ -> Nothing
            FailureResult (MkLimit fx) -> Just fx
    snfu :: SelectNotify (f ())
    snfu = mapMaybeSelectNotify getf snfsel
    in mkCVUISpec $ OneWholeUISpec subf spec snfu
