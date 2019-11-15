module Truth.Core.Resource.Function where

import Truth.Core.Import

data TransListFunction (tt1 :: [TransKind]) (tt2 :: [TransKind]) = MkTransListFunction
    { tlfFunction :: forall m. MonadIO m => Proxy m -> MFunction (ApplyStack tt1 m) (ApplyStack tt2 m)
    , tlfBackFunction :: forall m. MonadUnliftIO m => Proxy m -> MBackFunction (ApplyStack tt1 m) (ApplyStack tt2 m)
    }

instance Category TransListFunction where
    id = MkTransListFunction (\_ -> runWMFunction id) (\_ -> runWMBackFunction id)
    MkTransListFunction fbc bfbc . MkTransListFunction fab bfab =
        MkTransListFunction
            (\p -> runWMFunction $ MkWMFunction (fbc p) . MkWMFunction (fab p))
            (\p -> runWMBackFunction $ MkWMBackFunction (bfbc p) . MkWMBackFunction (bfab p))

fstTransListFunction ::
       forall tt1 tt2. (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2)
    => TransListFunction tt1 (Concat tt1 tt2)
fstTransListFunction = let
    tlfFunction ::
           forall m. Monad m
        => Proxy m
        -> MFunction (ApplyStack tt1 m) (ApplyStack (Concat tt1 tt2) m)
    tlfFunction _ = concatFstMFunction @tt1 @tt2 @m
    tlfBackFunction ::
           forall m. MonadUnliftIO m
        => Proxy m
        -> MBackFunction (ApplyStack tt1 m) (ApplyStack (Concat tt1 tt2) m)
    tlfBackFunction _ =
        case transStackConcatRefl @tt1 @tt2 @m of
            Refl ->
                case transStackDict @MonadUnliftIO @tt2 @m of
                    Dict -> stackLiftMBackFunction @tt1 $ stackLiftWithUnlift @tt2 @m
    in MkTransListFunction {..}

sndTransListFunction ::
       forall tt1 tt2. (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2)
    => TransListFunction tt2 (Concat tt1 tt2)
sndTransListFunction = let
    tlfFunction ::
           forall m. Monad m
        => Proxy m
        -> MFunction (ApplyStack tt2 m) (ApplyStack (Concat tt1 tt2) m)
    tlfFunction _ = concatSndMFunction @tt1 @tt2 @m
    tlfBackFunction ::
           forall m. MonadUnliftIO m
        => Proxy m
        -> MBackFunction (ApplyStack tt2 m) (ApplyStack (Concat tt1 tt2) m)
    tlfBackFunction _ =
        case transStackConcatRefl @tt1 @tt2 @m of
            Refl ->
                case transStackDict @MonadUnliftIO @tt2 @m of
                    Dict -> stackLiftWithUnlift @tt1
    in MkTransListFunction {..}

consTransListFunction ::
       forall tta ttb t. MonadTransSemiTunnel t
    => ListType (Compose Dict (MonadTransConstraint Monad)) tta
    -> ListType (Compose Dict (MonadTransConstraint Monad)) ttb
    -> TransListFunction tta ttb
    -> TransListFunction (t ': tta) (t ': ttb)
consTransListFunction wtta wttb (MkTransListFunction tf tbf) = let
    tf' :: forall m. MonadIO m
        => Proxy m
        -> MFunction (ApplyStack (t ': tta) m) (ApplyStack (t ': ttb) m)
    tf' pm =
        case (witTransStackDict @Monad @tta @m wtta, witTransStackDict @Monad @ttb @m wttb) of
            (Dict, Dict) -> remonad $ tf pm
    tbf' ::
           forall m. MonadUnliftIO m
        => Proxy m
        -> MBackFunction (ApplyStack (t ': tta) m) (ApplyStack (t ': ttb) m)
    tbf' pm =
        case (witTransStackDict @Monad @tta @m wtta, witTransStackDict @Monad @ttb @m wttb) of
            (Dict, Dict) -> liftMBackFunction $ tbf pm
    in MkTransListFunction tf' tbf'

reorderTransListFunction ::
       forall tta ttb t. MonadTransUnliftAll t
    => ListType (Compose Dict MonadTransUnliftAll) tta
    -> ListType (Compose Dict (MonadTransConstraint Monad)) ttb
    -> TransListFunction (Concat tta (t ': ttb)) (t ': Concat tta ttb)
reorderTransListFunction wtta wttb = let
    tlff ::
           forall tt m. (Monad m)
        => Proxy m
        -> ListType (Compose Dict MonadTransUnliftAll) tt
        -> ( WMFunction (ApplyStack (Concat tt (t ': ttb)) m) (t (ApplyStack (Concat tt ttb) m))
           , ApplyStack tt (ApplyStack ttb m) :~: ApplyStack (Concat tt ttb) m
           , Dict (Monad (ApplyStack tt (ApplyStack ttb m)))
           , ApplyStack tt (ApplyStack (t ': ttb) m) :~: ApplyStack (Concat tt (t ': ttb)) m
           , Dict (Monad (ApplyStack tt (ApplyStack (t ': ttb) m))))
    tlff _ NilListType =
        case witTransStackDict @Monad @ttb @m wttb of
            Dict -> (id, Refl, Dict, Refl, hasTransConstraint @Monad @t @(ApplyStack ttb m))
    tlff pm (ConsListType (Compose Dict) (w :: ListType _ tt0)) =
        case tlff pm w of
            (wmf, Refl, dm@Dict, Refl, dtm@Dict) ->
                case hasTransConstraint @Monad @t @(ApplyStack tt0 (ApplyStack ttb m)) of
                    Dict ->
                        ( MkWMFunction commuteT . liftWMFunction wmf
                        , Refl
                        , transConstraintDict dm
                        , Refl
                        , transConstraintDict dtm)
    tlfFunction ::
           forall m. MonadIO m
        => Proxy m
        -> MFunction (ApplyStack (Concat tta (t ': ttb)) m) (ApplyStack (t ': Concat tta ttb) m)
    tlfFunction pm =
        case tlff pm wtta -- $ representative @_ @(ListType (Compose Dict MonadTransUnliftAll)) @tta of
              of
            (MkWMFunction mf, _, _, _, _) -> mf
    tlfbf ::
           forall tt m. (Monad m)
        => Proxy m
        -> ListType (Compose Dict MonadTransUnliftAll) tt
        -> ( WMBackFunction (ApplyStack (Concat tt (t ': ttb)) m) (t (ApplyStack (Concat tt ttb) m))
           , ApplyStack tt (ApplyStack ttb m) :~: ApplyStack (Concat tt ttb) m
           , Dict (Monad (ApplyStack tt (ApplyStack ttb m)))
           , ApplyStack tt (ApplyStack (t ': ttb) m) :~: ApplyStack (Concat tt (t ': ttb)) m
           , Dict (Monad (ApplyStack tt (ApplyStack (t ': ttb) m))))
    tlfbf _ NilListType =
        case witTransStackDict @Monad @ttb @m wttb of
            Dict -> (id, Refl, Dict, Refl, hasTransConstraint @Monad @t @(ApplyStack ttb m))
    tlfbf pm (ConsListType (Compose Dict) (w :: ListType _ tt0)) =
        case tlfbf pm w of
            (wmbf, Refl, dm@Dict, Refl, dtm@Dict) ->
                case hasTransConstraint @Monad @t @(ApplyStack tt0 (ApplyStack ttb m)) of
                    Dict ->
                        ( MkWMBackFunction commuteTBack . liftWMBackFunction wmbf
                        , Refl
                        , transConstraintDict dm
                        , Refl
                        , transConstraintDict dtm)
    tlfBackFunction ::
           forall m. MonadUnliftIO m
        => Proxy m
        -> MBackFunction (ApplyStack (Concat tta (t ': ttb)) m) (ApplyStack (t ': Concat tta ttb) m)
    tlfBackFunction pm =
        case tlfbf pm wtta -- $ representative @_ @(ListType (Compose Dict MonadTransUnliftAll)) @tta of
              of
            (MkWMBackFunction mbf, _, _, _, _) -> mbf
    in MkTransListFunction {..}

contractTransListFunction ::
       forall tt t. MonadTransAskUnlift t
    => ListType (Compose Dict (MonadTransConstraint Monad)) tt
    -> TransListFunction (t ': (t ': tt)) (t ': tt)
contractTransListFunction wtt = let
    tlfFunction ::
           forall m. Monad m
        => Proxy m
        -> MFunction (ApplyStack (t ': (t ': tt)) m) (ApplyStack (t ': tt) m)
    tlfFunction _ =
        case witTransStackDict @Monad @tt @m wtt of
            Dict -> contractT
    tlfBackFunction ::
           forall m. MonadUnliftIO m
        => Proxy m
        -> MBackFunction (ApplyStack (t ': (t ': tt)) m) (ApplyStack (t ': tt) m)
    tlfBackFunction _ =
        case witTransStackDict @Monad @tt @m wtt of
            Dict -> contractTBack
    in MkTransListFunction {..}
