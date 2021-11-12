module Changes.Core.Resource.Function where

import Changes.Core.Import

data TransListFunction (tt1 :: [TransKind]) (tt2 :: [TransKind]) = MkTransListFunction
    { tlfFunction :: forall m. MonadIO m => Proxy m -> MFunction (ApplyStack tt1 m) (ApplyStack tt2 m)
    , tlfBackFunction :: forall m. MonadTunnelIO m => Proxy m -> MBackFunction (ApplyStack tt1 m) (ApplyStack tt2 m)
    }

instance Category TransListFunction where
    id = MkTransListFunction (\_ -> runWMFunction id) (\_ -> runWMBackFunction id)
    MkTransListFunction fbc bfbc . MkTransListFunction fab bfab =
        MkTransListFunction
            (\p -> runWMFunction $ MkWMFunction (fbc p) . MkWMFunction (fab p))
            (\p -> runWMBackFunction $ MkWMBackFunction (bfbc p) . MkWMBackFunction (bfab p))

fstTransListFunction ::
       forall tt1 tt2. (MonadTransStackUnlift tt1, MonadTransStackUnlift tt2)
    => TransListFunction tt1 (Concat tt1 tt2)
fstTransListFunction = let
    tlfFunction ::
           forall m. Monad m
        => Proxy m
        -> MFunction (ApplyStack tt1 m) (ApplyStack (Concat tt1 tt2) m)
    tlfFunction _ = concatFstMFunction @tt1 @tt2 @m
    tlfBackFunction ::
           forall m. MonadTunnelIO m
        => Proxy m
        -> MBackFunction (ApplyStack tt1 m) (ApplyStack (Concat tt1 tt2) m)
    tlfBackFunction _ =
        case transStackConcatRefl @tt1 @tt2 @m of
            Refl ->
                case transStackDict @MonadTunnelIO @tt2 @m of
                    Dict -> stackLiftMBackFunction @tt1 $ stackLiftWithUnlift @tt2 @m
    in MkTransListFunction {..}

sndTransListFunction ::
       forall tt1 tt2. (MonadTransStackUnlift tt1, MonadTransStackUnlift tt2)
    => TransListFunction tt2 (Concat tt1 tt2)
sndTransListFunction = let
    tlfFunction ::
           forall m. Monad m
        => Proxy m
        -> MFunction (ApplyStack tt2 m) (ApplyStack (Concat tt1 tt2) m)
    tlfFunction _ = concatSndMFunction @tt1 @tt2 @m
    tlfBackFunction ::
           forall m. MonadTunnelIO m
        => Proxy m
        -> MBackFunction (ApplyStack tt2 m) (ApplyStack (Concat tt1 tt2) m)
    tlfBackFunction _ =
        case transStackConcatRefl @tt1 @tt2 @m of
            Refl ->
                case transStackDict @MonadTunnelIO @tt2 @m of
                    Dict -> stackLiftWithUnlift @tt1
    in MkTransListFunction {..}

liftTransListFunction ::
       forall t tt. (MonadTransUnlift t, MonadTransStackUnlift tt)
    => TransListFunction tt (t ': tt)
liftTransListFunction = let
    tlfFunction ::
           forall m. Monad m
        => Proxy m
        -> MFunction (ApplyStack tt m) (ApplyStack (t ': tt) m)
    tlfFunction _ =
        case transStackDict @Monad @tt @m of
            Dict -> lift
    tlfBackFunction ::
           forall m. MonadTunnelIO m
        => Proxy m
        -> MBackFunction (ApplyStack tt m) (ApplyStack (t ': tt) m)
    tlfBackFunction _ =
        case transStackDict @MonadTunnelIO @tt @m of
            Dict -> liftWithUnlift
    in MkTransListFunction {..}

emptyTransListFunction ::
       forall tt. MonadTransStackUnlift tt
    => TransListFunction '[] tt
emptyTransListFunction = let
    tlfFunction ::
           forall m. Monad m
        => Proxy m
        -> MFunction m (ApplyStack tt m)
    tlfFunction _ = stackLift @tt @m
    tlfBackFunction ::
           forall m. MonadTunnelIO m
        => Proxy m
        -> MBackFunction m (ApplyStack tt m)
    tlfBackFunction _ = stackLiftWithUnlift @tt @m
    in MkTransListFunction {..}

consTransListFunction ::
       forall tta ttb t. TransTunnel t
    => ListType (Compose Dict (TransConstraint Monad)) tta
    -> ListType (Compose Dict (TransConstraint Monad)) ttb
    -> TransListFunction tta ttb
    -> TransListFunction (t ': tta) (t ': ttb)
consTransListFunction wtta wttb (MkTransListFunction tf tbf) = let
    tf' :: forall m. MonadIO m
        => Proxy m
        -> MFunction (ApplyStack (t ': tta) m) (ApplyStack (t ': ttb) m)
    tf' pm =
        case (witTransStackDict @Monad @tta @m wtta, witTransStackDict @Monad @ttb @m wttb) of
            (Dict, Dict) -> hoist $ tf pm
    tbf' ::
           forall m. MonadTunnelIO m
        => Proxy m
        -> MBackFunction (ApplyStack (t ': tta) m) (ApplyStack (t ': ttb) m)
    tbf' pm =
        case (witTransStackDict @Monad @tta @m wtta, witTransStackDict @Monad @ttb @m wttb) of
            (Dict, Dict) -> liftMBackFunction $ tbf pm
    in MkTransListFunction tf' tbf'

reorderTransListFunction ::
       forall tta ttb t. MonadTransUnlift t
    => ListType (Compose Dict MonadTransUnlift) tta
    -> ListType (Compose Dict (TransConstraint Monad)) ttb
    -> TransListFunction (Concat tta (t ': ttb)) (t ': Concat tta ttb)
reorderTransListFunction wtta wttb = let
    tlff ::
           forall tt m. (Monad m)
        => Proxy m
        -> ListType (Compose Dict MonadTransUnlift) tt
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
            (MkWMFunction mf, Refl, dm@Dict, Refl, dtm@Dict) ->
                case hasTransConstraint @Monad @t @(ApplyStack tt0 (ApplyStack ttb m)) of
                    Dict ->
                        ( MkWMFunction $ commuteT . hoist mf
                        , Refl
                        , transConstraintDict dm
                        , Refl
                        , transConstraintDict dtm)
    tlfFunction ::
           forall m. MonadIO m
        => Proxy m
        -> MFunction (ApplyStack (Concat tta (t ': ttb)) m) (ApplyStack (t ': Concat tta ttb) m)
    tlfFunction pm =
        case tlff pm wtta of
            (MkWMFunction mf, _, _, _, _) -> mf
    tlfbf ::
           forall tt m. (Monad m)
        => Proxy m
        -> ListType (Compose Dict MonadTransUnlift) tt
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
           forall m. MonadTunnelIO m
        => Proxy m
        -> MBackFunction (ApplyStack (Concat tta (t ': ttb)) m) (ApplyStack (t ': Concat tta ttb) m)
    tlfBackFunction pm =
        case tlfbf pm wtta of
            (MkWMBackFunction mbf, _, _, _, _) -> mbf
    in MkTransListFunction {..}

contractTransListFunction ::
       forall tt t. MonadTransAskUnlift t
    => ListType (Compose Dict (TransConstraint Monad)) tt
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
           forall m. MonadTunnelIO m
        => Proxy m
        -> MBackFunction (ApplyStack (t ': (t ': tt)) m) (ApplyStack (t ': tt) m)
    tlfBackFunction _ =
        case witTransStackDict @Monad @tt @m wtt of
            Dict -> contractTBack
    in MkTransListFunction {..}

stackTransListFunction :: forall tt. TransListFunction tt '[ StackT tt]
stackTransListFunction = let
    tlfFunction ::
           forall m. Monad m
        => Proxy m
        -> MFunction (ApplyStack tt m) (StackT tt m)
    tlfFunction _ = MkStackT
    tlfBackFunction ::
           forall m. MonadTunnelIO m
        => Proxy m
        -> MBackFunction (ApplyStack tt m) (StackT tt m)
    tlfBackFunction _ call = MkStackT $ call unStackT
    in MkTransListFunction {..}
