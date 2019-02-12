module Truth.Debug where

import Data.Time.Clock.System
import Debug.Trace
import System.IO.Unsafe
import Truth.Core.Edit.Edit
import Truth.Core.Edit.Function
import Truth.Core.Edit.Unlift
import Truth.Core.Import

contextStr :: String -> String -> String
contextStr "" b = b
contextStr a b = a ++ ": " ++ b

traceIOM :: MonadIO m => String -> m ()
traceIOM msg =
    liftIO $ do
        MkSystemTime s ns <- getSystemTime
        th <- myThreadId
        let
            showMod :: Int -> Word32 -> String
            showMod 0 _ = ""
            showMod n x = showMod (pred n) (div x 10) <> show (mod x 10)
        traceIO $ show s <> "." <> showMod 9 ns <> ": " <> show th <> ": " <> msg

traceBracketArgs :: MonadIO m => String -> String -> (r -> String) -> m r -> m r
traceBracketArgs s args showr ma = do
    traceIOM $
        s ++
        " [" ++
        (if null args
             then ""
             else " " ++ args)
    a <- ma
    let ret = showr a
    traceIOM $
        s ++
        " ]" ++
        (if null ret
             then ""
             else " => " ++ ret)
    return a

traceBracket :: MonadIO m => String -> m r -> m r
traceBracket s = traceBracketArgs s "" (\_ -> "")

tracePure :: String -> a -> a
tracePure s = seq (unsafePerformIO (traceIOM s))

tracePureBracket :: Monad m => String -> m a -> m a
tracePureBracket s ma = (tracePure (s ++ " [") ma) >>= (\a -> return $ tracePure (s ++ " ]") a)

class TraceThing t where
    traceThing :: String -> t -> t

instance MonadTransConstraint MonadIO t => TraceThing (Unlift t) where
    traceThing prefix unlift =
        MkUnlift $ \tma ->
            traceBracket (contextStr prefix "outside") $
            runUnlift unlift $ withTransConstraintTM @MonadIO $ traceBracket (contextStr prefix "inside") tma

instance MonadIO m => TraceThing (UnliftIO m) where
    traceThing prefix unlift =
        MkTransform $ \ma ->
            traceBracket (contextStr prefix "outside") $
            runTransform unlift $ traceBracket (contextStr prefix "inside") ma

class TraceArgThing t where
    traceArgThing :: String -> t -> t

type ShowableEdit edit
     = (Show edit, AllWitnessConstraint Show (EditReader edit), WitnessConstraint Show (EditReader edit))

class TraceAThing f where
    traceAThing ::
           forall (t :: (Type -> Type) -> (Type -> Type)) (edita :: Type) (editb :: Type). MonadTransUnlift t
        => String
        -> f t edita editb
        -> f t edita editb
    traceArgAThing ::
           forall (t :: (Type -> Type) -> (Type -> Type)) (edita :: Type) (editb :: Type).
           (MonadTransUnlift t, ShowableEdit edita, ShowableEdit editb)
        => String
        -> f t edita editb
        -> f t edita editb

instance TraceAThing f => TraceThing (CloseUnlift f a b) where
    traceThing prefix (MkCloseUnlift unlift athing) =
        MkCloseUnlift (traceThing prefix unlift) (traceAThing prefix athing)

instance (TraceAThing f, ShowableEdit edita, ShowableEdit editb) => TraceArgThing (CloseUnlift f edita editb) where
    traceArgThing prefix (MkCloseUnlift unlift athing) =
        MkCloseUnlift (traceThing prefix unlift) (traceArgAThing prefix athing)

instance TraceAThing AnEditFunction where
    traceAThing prefix (MkAnEditFunction g u) =
        MkAnEditFunction
            (\mr rt -> withTransConstraintTM @MonadIO $ traceBracket (contextStr prefix "get") $ g mr rt)
            (\ee mr -> withTransConstraintTM @MonadIO $ traceBracket (contextStr prefix "update") $ u ee mr)
    traceArgAThing ::
           forall t edita editb. (MonadTransUnlift t, ShowableEdit edita, ShowableEdit editb)
        => String
        -> AnEditFunction t edita editb
        -> AnEditFunction t edita editb
    traceArgAThing prefix (MkAnEditFunction g u) =
        MkAnEditFunction
            (\mr (rt :: EditReader editb r) ->
                 case allWitnessConstraint @_ @_ @Show @(EditReader editb) @r of
                     Dict ->
                         case witnessConstraint @_ @Show rt of
                             Dict ->
                                 withTransConstraintTM @MonadIO $
                                 traceBracketArgs (contextStr prefix "get") (show rt) show $ g mr rt)
            (\ee mr ->
                 withTransConstraintTM @MonadIO $ traceBracketArgs (contextStr prefix "update") (show ee) show $ u ee mr)
