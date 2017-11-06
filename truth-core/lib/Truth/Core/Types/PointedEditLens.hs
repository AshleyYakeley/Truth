module Truth.Core.Types.PointedEditLens where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.Context
import Truth.Core.Types.Lattice
import Truth.Core.Types.Pair
import Truth.Core.Types.Tuple
import Truth.Core.Types.Whole

newtype PointedEditFunction editp edita editb =
    MkPointedEditFunction (PureEditFunction (ContextEdit editp edita) editb)

instance ConstrainedCategory (PointedEditFunction editp) where
    type CategoryConstraint (PointedEditFunction editp) edit = ()
    cid =
        let editAccess :: IOStateAccess ()
            editAccess = unitStateAccess
            editGet :: () -> ReadFunction (ContextEditReader editp edit) (EditReader edit)
            editGet () rt = readable $ MkTupleEditReader EditContent rt
            editUpdate :: ContextEdit editp edit -> () -> Readable (ContextEditReader editp edit) ((), [edit])
            editUpdate (MkTupleEdit EditContext _) () = pure $ pure []
            editUpdate (MkTupleEdit EditContent edit) () = pure $ pure $ pure edit
        in MkPointedEditFunction $ MkEditFunction {..}
    (MkPointedEditFunction (funcBC :: PureEditFunction (ContextEdit editp editb) editc)) <.> (MkPointedEditFunction (funcAB :: PureEditFunction (ContextEdit editp edita) editb)) =
        let convAB :: () -> ReadFunction (ContextEditReader editp edita) (ContextEditReader editp editb)
            convAB () (MkTupleEditReader EditContext rt) = readable $ MkTupleEditReader EditContext rt
            convAB () (MkTupleEditReader EditContent rt) = editGet funcAB () rt
        in MkPointedEditFunction $
           MkEditFunction
           { editAccess = unitStateAccess
           , editGet = \() readC -> mapGenReadable (convAB ()) $ editGet funcBC () readC
           , editUpdate =
                 \editpa () -> do
                     ((), editBs) <- editUpdate funcAB editpa ()
                     (midBC, peditCs) <-
                         case editpa of
                             MkTupleEdit EditContext editP ->
                                 mapGenReadable (convAB ()) $ editUpdate funcBC (MkTupleEdit EditContext editP) ()
                             MkTupleEdit EditContent _ -> return ((), [])
                     ((), editCs) <-
                         mapGenReadable (convAB ()) $ editUpdates funcBC (fmap (MkTupleEdit EditContent) editBs) midBC
                     return ((), peditCs ++ editCs)
           }

newtype PointedEditLens editp edita editb =
    MkPointedEditLens (PureEditLens (ContextEdit editp edita) editb)

instance Edit editp => ConstrainedCategory (PointedEditLens editp) where
    type CategoryConstraint (PointedEditLens editp) edit = Edit edit
    cid =
        let MkPointedEditFunction editLensFunction = cid
            editLensPutEdit ::
                   () -> edit -> Readable (ContextEditReader editp edit) (Maybe ((), [ContextEdit editp edit]))
            editLensPutEdit () edit = pure $ pure $ pure $ pure $ MkTupleEdit EditContent edit
        in MkPointedEditLens $ MkEditLens {..}
    (MkPointedEditLens (lensBC :: PureEditLens (ContextEdit editp editb) editc)) <.> (MkPointedEditLens (lensAB :: PureEditLens (ContextEdit editp edita) editb)) =
        let funcAB = editLensFunction lensAB
            funcBC = editLensFunction lensBC
            pesAB = editLensPutEdits lensAB
            peBC = editLensPutEdit lensBC
            convAB :: () -> ReadFunction (ContextEditReader editp edita) (ContextEditReader editp editb)
            convAB () (MkTupleEditReader EditContext rt) = readable $ MkTupleEditReader EditContext rt
            convAB () (MkTupleEditReader EditContent rt) = editGet funcAB () rt
        in let MkPointedEditFunction editLensFunction = MkPointedEditFunction funcBC <.> MkPointedEditFunction funcAB
               editLensPutEdit () editC = do
                   fslb <- mapGenReadable (convAB ()) $ peBC () editC
                   ff <-
                       for fslb $ \((), editPBs) ->
                           let editPsBs :: forall t. WithContextSelector editp editb t -> [t]
                               editPsBs = getAllF $ splitTupleEditList editPBs
                               editPs = editPsBs EditContext
                               editBs = editPsBs EditContent
                           in do fsla <- pesAB () $ editBs
                                 return $
                                     fmap
                                         (\((), editPAs) -> ((), (fmap (MkTupleEdit EditContext) editPs) ++ editPAs))
                                         fsla
                   return $ ff >>= id
           in MkPointedEditLens $ MkEditLens {..}

pointedWholeFunctionRead :: PointedEditFunction editp (WholeEdit a) (WholeEdit b) -> a -> Readable (EditReader editp) b
pointedWholeFunctionRead (MkPointedEditFunction MkEditFunction {..}) a =
    mapReadable
        (\case
             MkTupleEditReader EditContext rt -> readable rt
             MkTupleEditReader EditContent ReadWhole -> return a) $
    editGet () ReadWhole

funcPointedEditFunction ::
       forall editp edita editb. (Edit edita, FullSubjectReader (EditReader edita), FullEdit editb)
    => (EditSubject edita -> EditSubject editb)
    -> PointedEditFunction editp edita editb
funcPointedEditFunction ab = editFunctionToPointed $ funcEditFunction ab

pointedEditLensFunction :: PointedEditLens editp edita editb -> PointedEditFunction editp edita editb
pointedEditLensFunction (MkPointedEditLens lens) = MkPointedEditFunction $ editLensFunction lens

readOnlyPointedEditLens :: PointedEditFunction editp edita editb -> PointedEditLens editp edita editb
readOnlyPointedEditLens (MkPointedEditFunction f) = MkPointedEditLens $ readOnlyEditLens f

editFunctionToPointed :: PureEditFunction edita editb -> PointedEditFunction editp edita editb
editFunctionToPointed f =
    case cid of
        MkPointedEditFunction idf -> MkPointedEditFunction $ f <.> idf

editLensToPointed ::
       (Edit editp, Edit edita, Edit editb) => PureEditLens edita editb -> PointedEditLens editp edita editb
editLensToPointed lens =
    case cid of
        MkPointedEditLens idlens -> MkPointedEditLens $ lens <.> idlens

composeEditLensPointed ::
       (Edit editp, Edit edita, Edit editb, Edit editc)
    => PureEditLens editb editc
    -> PointedEditLens editp edita editb
    -> PointedEditLens editp edita editc
composeEditLensPointed lensBC (MkPointedEditLens lensAB) = MkPointedEditLens $ lensBC <.> lensAB

instance (JoinSemiLatticeEdit editb, Edit editp, Edit edita, Edit editb) =>
         JoinSemiLattice (PointedEditFunction editp edita editb) where
    (MkPointedEditFunction f1) \/ (MkPointedEditFunction f2) =
        MkPointedEditFunction $ joinEditFunction <.> editToPureEditFunction (pairJoinEditFunctions f1 f2)

instance (MeetSemiLatticeEdit editb, Edit editp, Edit edita, Edit editb) =>
         MeetSemiLattice (PointedEditFunction editp edita editb) where
    (MkPointedEditFunction f1) /\ (MkPointedEditFunction f2) =
        MkPointedEditFunction $ meetEditFunction <.> editToPureEditFunction (pairJoinEditFunctions f1 f2)

carryPointedEditLens ::
       (Edit editx, Edit edita, Edit editb)
    => PointedEditLens editx edita editb
    -> GeneralLens (ContextEdit editx edita) (ContextEdit editx editb)
carryPointedEditLens (MkPointedEditLens lens) = carryContextGeneralLens $ MkCloseState lens

pointedMapGeneralFunction ::
       (Edit editx, Edit edita, Edit editb)
    => PointedEditFunction editx edita editb
    -> GeneralFunction editx edita
    -> GeneralFunction editx editb
pointedMapGeneralFunction (MkPointedEditFunction ef) efxa = (MkCloseState ef) <.> contextualiseGeneralFunction efxa

pointedMapGeneralLens ::
       (Edit editx, Edit edita, Edit editb)
    => PointedEditLens editx edita editb
    -> GeneralLens editx edita
    -> GeneralLens editx editb
pointedMapGeneralLens (MkPointedEditLens lens) lensxa = (MkCloseState lens) <.> contextualiseGeneralLens lensxa
