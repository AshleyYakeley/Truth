module Truth.Core.Resource.Runnable where

import Truth.Core.Import
import Truth.Core.Resource.Function
import Truth.Core.Resource.Runner

data Runnable1 f (a :: k) =
    forall (tt :: [TransKind]). MkRunnable1 (TransStackRunner tt)
                                            (f tt a)

class RunnableMap (f :: [TransKind] -> k) where
    mapRunnable ::
           forall tt1 tt2. (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2)
        => TransListFunction tt1 tt2
        -> KindFunction (f tt1) (f tt2)

joinRunnable1Maps_ ::
       forall k1 k2 f1 f2 (a1 :: k1) (a2 :: k2) r. (InKind a1, InKind a2, RunnableMap f1, RunnableMap f2)
    => (forall tt. MonadTransStackUnliftAll tt => f1 tt a1 -> f2 tt a2 -> TransStackRunner tt -> r)
    -> Runnable1 f1 a1
    -> Runnable1 f2 a2
    -> r
joinRunnable1Maps_ ff (MkRunnable1 (run1 :: TransStackRunner tt1) fma1) (MkRunnable1 (run2 :: TransStackRunner tt2) fma2) =
    case transStackRunnerUnliftAllDict run1 of
        Dict ->
            case transStackRunnerUnliftAllDict run2 of
                Dict ->
                    case concatMonadTransStackUnliftAllDict @tt1 @tt2 of
                        Dict ->
                            ff
                                @(Concat tt1 tt2)
                                ((unNestedMorphism $ mapRunnable $ fstTransListFunction @tt1 @tt2) fma1)
                                ((unNestedMorphism $ mapRunnable $ sndTransListFunction @tt1 @tt2) fma2)
                                (cmAppend run1 run2)

joinRunnable1Maps ::
       forall k1 k2 k3 f1 f2 f3 (a1 :: k1) (a2 :: k2) (a3 :: k3). (InKind a1, InKind a2, RunnableMap f1, RunnableMap f2)
    => (forall tt. MonadTransStackUnliftAll tt => f1 tt a1 -> f2 tt a2 -> f3 tt a3)
    -> Runnable1 f1 a1
    -> Runnable1 f2 a2
    -> Runnable1 f3 a3
joinRunnable1Maps ff = joinRunnable1Maps_ $ \f1 f2 run -> MkRunnable1 run $ ff f1 f2

data Runnable2 f (a :: k1) (b :: k2) =
    forall (tt :: [TransKind]). MkRunnable2 (TransStackRunner tt)
                                            (f tt a b)

joinRunnable2s ::
       (forall tt1 tt2.
            (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2, MonadTransStackUnliftAll (Concat tt1 tt2)) =>
                    f1 tt1 a1 b1 -> f2 tt2 a2 b2 -> f3 (Concat tt1 tt2) a3 b3)
    -> Runnable2 f1 a1 b1
    -> Runnable2 f2 a2 b2
    -> Runnable2 f3 a3 b3
joinRunnable2s call (MkRunnable2 (run1 :: TransStackRunner tt1) open1) (MkRunnable2 (run2 :: TransStackRunner tt2) open2) =
    case transStackRunnerUnliftAllDict run1 of
        Dict ->
            case transStackRunnerUnliftAllDict run2 of
                Dict ->
                    MkRunnable2 (cmAppend run1 run2) $
                    case concatMonadTransStackUnliftAllDict @tt1 @tt2 of
                        Dict -> call open1 open2

class RunnableCategory (f :: [TransKind] -> k -> k -> Type) where
    ucId :: forall a. f '[] a a
    ucCompose ::
           forall tab tbc a b c. (MonadTransStackUnliftAll tab, MonadTransStackUnliftAll tbc)
        => f tbc b c
        -> f tab a b
        -> f (Concat tbc tab) a c

instance RunnableCategory f => Category (Runnable2 f) where
    id = MkRunnable2 cmEmpty ucId
    (.) = joinRunnable2s ucCompose

joinRunnable2Maps ::
       forall f1 f2 f3 a1 b1 a2 b2 a3 b3. (InKind a1, InKind a2, InKind b1, InKind b2, RunnableMap f1, RunnableMap f2)
    => (forall tt. MonadTransStackUnliftAll tt => f1 tt a1 b1 -> f2 tt a2 b2 -> f3 tt a3 b3)
    -> Runnable2 f1 a1 b1
    -> Runnable2 f2 a2 b2
    -> Runnable2 f3 a3 b3
joinRunnable2Maps call =
    joinRunnable2s $ \(open1 :: f1 tt1 a1 b1) (open2 :: f2 tt2 a2 b2) ->
        call
            @(Concat tt1 tt2)
            ((unNestedMorphism $ unNestedMorphism $ mapRunnable $ fstTransListFunction @tt1 @tt2) open1)
            ((unNestedMorphism $ unNestedMorphism $ mapRunnable $ sndTransListFunction @tt1 @tt2) open2)

joinRunnable211Maps ::
       forall ka1 kb1 ka2 ka3 f1 f2 f3 (a1 :: ka1) (b1 :: kb1) (a2 :: ka2) (a3 :: ka3).
       (InKind a1, InKind b1, InKind a2, RunnableMap f1, RunnableMap f2)
    => (forall tt. MonadTransStackUnliftAll tt => f1 tt a1 b1 -> f2 tt a2 -> f3 tt a3)
    -> Runnable2 f1 a1 b1
    -> Runnable1 f2 a2
    -> Runnable1 f3 a3
joinRunnable211Maps ff (MkRunnable2 (trun1 :: TransStackRunner tt1) fma1) (MkRunnable1 (trun2 :: TransStackRunner tt2) fma2) =
    case transStackRunnerUnliftAllDict trun1 of
        Dict ->
            case transStackRunnerUnliftAllDict trun2 of
                Dict ->
                    MkRunnable1 (cmAppend trun1 trun2) $
                    case concatMonadTransStackUnliftAllDict @tt1 @tt2 of
                        Dict ->
                            ff
                                @(Concat tt1 tt2)
                                ((unNestedMorphism $ unNestedMorphism $ mapRunnable $ fstTransListFunction @tt1 @tt2)
                                     fma1)
                                ((unNestedMorphism $ mapRunnable $ sndTransListFunction @tt1 @tt2) fma2)

exclusiveRunnable1 :: Runnable1 f a -> LifeCycleIO (Runnable1 f a)
exclusiveRunnable1 (MkRunnable1 trun f) = do
    trun' <- exclusiveTransStackRunner trun
    return $ MkRunnable1 trun' f
