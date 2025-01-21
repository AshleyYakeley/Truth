{-# OPTIONS -fno-warn-orphans #-}

module Changes.Debug.Reference
    ( module Changes.Debug
    , module Changes.Debug.Edit
    , module Changes.Debug.Reference
    )
where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Model.EditContext
import Changes.Core.Model.Reference
import Changes.Core.Read
import Changes.Core.Resource
import Changes.Debug
import Changes.Debug.Edit

data EditShower edit = MkEditShower
    { showRead :: forall t. EditReader edit t -> String
    , showReadResult :: forall t. EditReader edit t -> t -> String
    , showEdit :: edit -> String
    }

blankEditShower :: EditShower edit
blankEditShower = MkEditShower{showRead = \_ -> "", showReadResult = \_ _ -> "", showEdit = \_ -> "edit"}

traceAReference ::
    forall tt edit.
    MonadIO (ApplyStack tt IO) =>
    String ->
    EditShower edit ->
    AReference edit tt ->
    AReference edit tt
traceAReference prefix MkEditShower{..} (MkAReference r e ct) = let
    r' :: Readable (ApplyStack tt IO) (EditReader edit)
    r' rt = traceBracketArgs (contextStr prefix "read") (showRead rt) (showReadResult rt) $ r rt
    e' :: NonEmpty edit -> ApplyStack tt IO (Maybe (EditSource -> ApplyStack tt IO ()))
    e' edits =
        traceBracketArgs
            (contextStr prefix "edit.examine")
            ("[" ++ intercalate "," (toList $ fmap showEdit edits) ++ "]")
            ( \mx ->
                if isJust mx
                    then "action"
                    else "no action"
            )
            $ ( fmap
                    $ fmap
                    $ fmap
                    $ traceBracketArgs
                        (contextStr prefix "edit.do")
                        ("[" ++ intercalate "," (toList $ fmap showEdit edits) ++ "]")
                        (\_ -> "")
              )
            $ e edits
    in MkAReference r' e' ct

traceReference :: forall edit. String -> EditShower edit -> Reference edit -> Reference edit
traceReference prefix shower (MkResource rr anobj) =
    case resourceRunnerStackUnliftDict @IO rr of
        Dict -> MkResource rr $ traceAReference prefix shower anobj

showEditShower ::
    forall edit.
    ShowableEdit edit =>
    EditShower edit
showEditShower = let
    showRead rt = allShow rt
    showReadResult :: forall t. EditReader edit t -> t -> String
    showReadResult rt t =
        case witnessConstraint @_ @Show rt of
            Dict -> show t
    showEdit = show
    in MkEditShower{..}

instance TraceThing (ChangeLens updateA updateB) where
    traceThing prefix (MkChangeLens g u pe) =
        MkChangeLens
            (\mr rt -> traceBracket_ (contextStr prefix "get") $ g mr rt)
            (\ee mr -> traceBracket_ (contextStr prefix "update") $ u ee mr)
            ( \ee mr ->
                traceBracket_ (contextStr prefix "put") $ do
                    mee <- pe ee mr
                    case mee of
                        Just _ -> traceIOM (contextStr prefix "put: edits")
                        Nothing -> traceIOM (contextStr prefix "put: no edits")
                    return mee
            )

instance (ShowableUpdate updateA, ShowableUpdate updateB) => TraceArgThing (ChangeLens updateA updateB) where
    traceArgThing prefix (MkChangeLens g u pe) =
        MkChangeLens
            ( \mr (rt :: UpdateReader updateB r) ->
                case allConstraint @_ @_ @Show @(UpdateReader updateB) @r of
                    Dict ->
                        case witnessConstraint @_ @Show rt of
                            Dict -> traceBracketArgs (contextStr prefix "get") (show rt) show $ g mr rt
            )
            (\ee mr -> traceBracketArgs (contextStr prefix "update") (show ee) show $ u ee mr)
            ( \ee mr ->
                traceBracketArgs (contextStr prefix "put") (show ee) show $ do
                    mee <- pe ee mr
                    case mee of
                        Just _ -> traceIOM (contextStr prefix "put: edits")
                        Nothing -> traceIOM (contextStr prefix "put: no edits")
                    return mee
            )

instance TraceThing (Reference edit) where
    traceThing prefix = traceReference prefix blankEditShower

instance ShowableEdit edit => TraceArgThing (Reference edit) where
    traceArgThing prefix = traceReference prefix showEditShower

instance TraceThing LifeState where
    traceThing _ ls = ls

slowObject :: Int -> Reference edit -> Reference edit
slowObject mus (MkResource rr (MkAReference rd push ct)) =
    case resourceRunnerStackUnliftDict @IO rr of
        Dict -> let
            push' edits = do
                maction <- push edits
                return
                    $ case maction of
                        Nothing -> Nothing
                        Just action ->
                            Just $ \esrc -> do
                                traceBracket_ "slow: delay" $ liftIO $ threadDelay mus
                                traceBracket_ "slow: action" $ action esrc
            in MkResource rr $ MkAReference rd push' ct

instance TraceThing (FloatingChangeLens updateA updateB) where
    traceThing prefix (MkFloatingChangeLens finit lens) = let
        finit' =
            case finit of
                NoFloatInit r -> NoFloatInit r
                ReadFloatInit fi -> ReadFloatInit $ \mr -> traceBracket_ (contextStr prefix "finit") $ fi mr
        lens' r = traceThing prefix $ lens r
        in MkFloatingChangeLens finit' lens'
