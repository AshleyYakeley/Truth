module Truth.Core.Types.PointedEditLens where
{-
import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Context
import Truth.Core.Types.Tuple

--import Truth.Core.Types.Lattice
--import Truth.Core.Types.Pair
--import Truth.Core.Types.Whole

newtype PointedEditFunction editp edita editb =
    MkPointedEditFunction (EditFunction' (ContextEdit editp edita) editb)

instance ConstrainedCategory (PointedEditFunction editp) where
    type CategoryConstraint (PointedEditFunction editp) edit = ()
    cid = MkPointedEditFunction $ editLensFunction $ tupleEditLens EditContent
    (MkPointedEditFunction funcBC) <.> (MkPointedEditFunction funcAB) =
        MkPointedEditFunction $ funcBC <.> carryContextEditFunction funcAB

newtype PointedEditLens editp edita editb =
    MkPointedEditLens (EditLens' (ContextEdit editp edita) editb)

instance Edit editp => ConstrainedCategory (PointedEditLens editp) where
    type CategoryConstraint (PointedEditLens editp) edit = Edit edit
    cid = MkPointedEditLens $ tupleEditLens EditContent
    (MkPointedEditLens lensBC) <.> (MkPointedEditLens lensAB) =
        MkPointedEditLens $ lensBC <.> carryContextEditLens lensAB

funcPointedEditFunction ::
       forall editp edita editb. (Edit edita, FullSubjectReader (EditReader edita), FullEdit editb)
    => (EditSubject edita -> EditSubject editb)
    -> PointedEditFunction editp edita editb
funcPointedEditFunction ab = editFunctionToPointed $ funcEditFunction ab

pointedEditLensFunction :: PointedEditLens editp edita editb -> PointedEditFunction editp edita editb
pointedEditLensFunction (MkPointedEditLens lens) = MkPointedEditFunction $ editLensFunction lens

readOnlyPointedEditLens :: PointedEditFunction editp edita editb -> PointedEditLens editp edita editb
readOnlyPointedEditLens (MkPointedEditFunction f) = MkPointedEditLens $ readOnlyEditLens f

editFunctionToPointed :: EditFunction' edita editb -> PointedEditFunction editp edita editb
editFunctionToPointed f =
    case cid of
        MkPointedEditFunction idf -> MkPointedEditFunction $ f <.> idf

editLensToPointed :: (Edit editp, Edit edita, Edit editb) => EditLens' edita editb -> PointedEditLens editp edita editb
editLensToPointed lens =
    case cid of
        MkPointedEditLens idlens -> MkPointedEditLens $ lens <.> idlens

composeEditLensPointed ::
       (Edit editp, Edit edita, Edit editb, Edit editc)
    => EditLens' editb editc
    -> PointedEditLens editp edita editb
    -> PointedEditLens editp edita editc
composeEditLensPointed lensBC (MkPointedEditLens lensAB) = MkPointedEditLens $ lensBC <.> lensAB

{-
instance (JoinSemiLatticeEdit editb, Edit editp, Edit edita, Edit editb) =>
         JoinSemiLattice (PointedEditFunction editp edita editb) where
    (MkPointedEditFunction f1) \/ (MkPointedEditFunction f2) =
        MkPointedEditFunction $ joinEditFunction <.> editToPureEditFunction (pairJoinEditFunctions f1 f2)

instance (MeetSemiLatticeEdit editb, Edit editp, Edit edita, Edit editb) =>
         MeetSemiLattice (PointedEditFunction editp edita editb) where
    (MkPointedEditFunction f1) /\ (MkPointedEditFunction f2) =
        MkPointedEditFunction $ meetEditFunction <.> editToPureEditFunction (pairJoinEditFunctions f1 f2)
-}
carryPointedEditLens ::
       (Edit editx, Edit edita, Edit editb)
    => PointedEditLens editx edita editb
    -> EditLens' (ContextEdit editx edita) (ContextEdit editx editb)
carryPointedEditLens (MkPointedEditLens lens) = carryContextEditLens lens

pointedMapEditFunction ::
       PointedEditFunction editx edita editb -> EditFunction' editx edita -> EditFunction' editx editb
pointedMapEditFunction (MkPointedEditFunction ef) efxa = ef <.> contextualiseEditFunction efxa

pointedMapEditLens ::
       (Edit editx, Edit edita, Edit editb)
    => PointedEditLens editx edita editb
    -> EditLens' editx edita
    -> EditLens' editx editb
pointedMapEditLens (MkPointedEditLens lens) lensxa = lens <.> contextualiseEditLens lensxa
-}