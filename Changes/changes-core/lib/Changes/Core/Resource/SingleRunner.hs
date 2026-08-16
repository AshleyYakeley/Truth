module Changes.Core.Resource.SingleRunner
    ( SingleRunner
    , mkSingleRunner
    , runSingleRunner
    )
where

import Changes.Core.Import

data SingleRunner (t :: Type) = MkSingleRunner (IOWitness t) (WithT IO t)

instance TestEquality SingleRunner where
    testEquality (MkSingleRunner wa _) (MkSingleRunner wb _) = testEquality wa wb

instance TestOrder SingleRunner where
    testCompare (MkSingleRunner wa _) (MkSingleRunner wb _) = testCompare wa wb

mkSingleRunner ::
    forall (t :: Type).
    IOWitness t ->
    With IO t ->
    SingleRunner t
mkSingleRunner wit ww = MkSingleRunner wit $ MkWithT ww

fetchInSomeList :: TestEquality w => [Some w] -> w t -> Maybe (w t, w t -> [Some w])
fetchInSomeList [] _ = Nothing
fetchInSomeList (MkSome it : aa) wt
    | Just Refl <- testEquality it wt = Just (it, \it' -> MkSome it' : aa)
fetchInSomeList (a : aa) wt = do
    (it, f) <- fetchInSomeList aa wt
    return (it, \it' -> a : f it')

fetchSingleRunner ::
    forall t.
    [Some SingleRunner] ->
    SingleRunner t ->
    (WithT IO t -> [Some SingleRunner], WithT IO t)
fetchSingleRunner rr sr@(MkSingleRunner swit srun) =
    case fetchInSomeList rr sr of
        Nothing -> (\run -> MkSome (MkSingleRunner swit run) : rr, srun)
        Just (MkSingleRunner cwit crun, f) -> (\run -> f (MkSingleRunner cwit run), crun)

runSingleRunner ::
    forall t r.
    [Some SingleRunner] ->
    SingleRunner t ->
    ([Some SingleRunner] -> t -> IO r) ->
    IO r
runSingleRunner rr sr call = let
    (rr', run) = fetchSingleRunner rr sr
    in unWithT (liftIOWithT run) $ \t -> call (rr' $ pure t) t
