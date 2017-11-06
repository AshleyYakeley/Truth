module Truth.Core.Edit.General where

import Truth.Core.Edit.Edit
import Truth.Core.Edit.EditFunction
import Truth.Core.Edit.EditLens
import Truth.Core.Edit.FullEdit
import Truth.Core.Import
import Truth.Core.Read

class StateCategory ff where
    identityState ::
           forall a. Edit a
        => ff () a a
    composeState ::
           forall a b c s1 s2. (Edit a, Edit b, Edit c)
        => ff s2 b c
        -> ff s1 a b
        -> ff (s1, s2) a c

data CloseState ff a b =
    forall state. MkCloseState (ff state a b)

instance StateCategory ff => ConstrainedCategory (CloseState ff) where
    type CategoryConstraint (CloseState ff) t = Edit t
    cid = MkCloseState identityState
    (MkCloseState bc) <.> (MkCloseState ab) = MkCloseState $ composeState bc ab

instance StateCategory EditFunction where
    identityState = cid
    composeState fef2 fef1 =
        MkEditFunction
        { editAccess = pairStateAccess (editAccess fef1) (editAccess fef2)
        , editGet = \(s1, s2) -> composeReadFunction (editGet fef2 s2) (editGet fef1 s1)
        , editUpdate =
              \editA (oldstate1, oldstate2) -> do
                  (newstate1, editBs) <- editUpdate fef1 editA oldstate1
                  (newstate2, editCs) <- mapGenReadable (editGet fef1 oldstate1) $ editUpdates fef2 editBs oldstate2
                  return ((newstate1, newstate2), editCs)
        }

instance StateCategory EditLens where
    identityState =
        let editLensFunction = identityState
            editLensPutEdit st edit = pure $ pure (st, [edit])
        in MkEditLens {..}
    composeState fel2 fel1 =
        MkEditLens
        { editLensFunction = composeState (editLensFunction fel2) (editLensFunction fel1)
        , editLensPutEdit =
              \(olds1, olds2) editc -> do
                  meditb <- mapGenReadable (editGet (editLensFunction fel1) olds1) (editLensPutEdit fel2 olds2 editc)
                  case retrieveOne meditb of
                      SuccessResult (news2, editbs) -> do
                          mn1ea <- editLensPutEdits fel1 olds1 editbs
                          return $ fmap (\(news1, edita) -> ((news1, news2), edita)) mn1ea
                      FailureResult (MkLimit mx) -> return mx
        }

type GeneralFunction = CloseState EditFunction

type GeneralLens = CloseState EditLens

class IsGeneralLens lens where
    type LensDomain lens :: *
    type LensRange lens :: *
    toGeneralLens :: lens -> GeneralLens (LensDomain lens) (LensRange lens)

instance IsGeneralLens (GeneralLens edita editb) where
    type LensDomain (GeneralLens edita editb) = edita
    type LensRange (GeneralLens edita editb) = editb
    toGeneralLens = id

instance IsGeneralLens (EditLens state edita editb) where
    type LensDomain (EditLens state edita editb) = edita
    type LensRange (EditLens state edita editb) = editb
    toGeneralLens = MkCloseState

generalLensFunction :: forall edita editb. GeneralLens edita editb -> GeneralFunction edita editb
generalLensFunction (MkCloseState (MkEditLens ef _)) = MkCloseState ef

readOnlyGeneralLens :: forall edita editb. GeneralFunction edita editb -> GeneralLens edita editb
readOnlyGeneralLens (MkCloseState ef) = MkCloseState $ readOnlyEditLens ef

funcGeneralFunction ::
       forall edita editb. (Edit edita, FullSubjectReader (EditReader edita), FullEdit editb)
    => (EditSubject edita -> EditSubject editb)
    -> GeneralFunction edita editb
funcGeneralFunction = MkCloseState . funcEditFunction

convertGeneralFunction ::
       forall edita editb.
       (EditSubject edita ~ EditSubject editb, Edit edita, FullSubjectReader (EditReader edita), FullEdit editb)
    => GeneralFunction edita editb
convertGeneralFunction = MkCloseState convertEditFunction

funcROGeneralLens ::
       forall edita editb. (Edit edita, FullSubjectReader (EditReader edita), FullEdit editb)
    => (EditSubject edita -> EditSubject editb)
    -> GeneralLens edita editb
funcROGeneralLens = readOnlyGeneralLens . funcGeneralFunction

constGeneralFunction ::
       forall edita editb. (SubjectReader (EditReader editb))
    => EditSubject editb
    -> GeneralFunction edita editb
constGeneralFunction = MkCloseState . constEditFunction

constGeneralLens ::
       forall edita editb. (SubjectReader (EditReader editb))
    => EditSubject editb
    -> GeneralLens edita editb
constGeneralLens = readOnlyGeneralLens . constGeneralFunction

convertGeneralLens ::
       forall edita editb. (EditSubject edita ~ EditSubject editb, FullEdit edita, FullEdit editb)
    => GeneralLens edita editb
convertGeneralLens = MkCloseState convertEditLens
