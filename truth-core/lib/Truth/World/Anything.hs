module Truth.World.Anything where

import Truth.Core
import Truth.Core.Import

data Anything where
    MkAnything :: forall (edit :: Type). IOWitness edit -> EditSubject edit -> Anything

data AnyTypes where
    MkAnyTypes :: forall (edit :: Type). IOWitness edit -> AnyTypes

data AnyReader t where
    ReadAnyTypes :: AnyReader AnyTypes
    ReadAnyReader
        :: forall edit t. SubjectReader (EditReader edit)
        => IOWitness edit
        -> EditReader edit t
        -> AnyReader (Maybe t)

instance SubjectReader AnyReader where
    type ReaderSubject AnyReader = Anything
    subjectToRead (MkAnything wit _a) ReadAnyTypes = MkAnyTypes wit
    subjectToRead (MkAnything wit a) (ReadAnyReader witr reader) = do
        Refl <- testEquality wit witr
        return (subjectToRead a reader)

data AnyEdit where
    MkAnyEdit
        :: forall edit. (ApplicableEdit edit, InvertibleEdit edit, SubjectReader (EditReader edit))
        => IOWitness edit
        -> edit
        -> AnyEdit

instance Floating AnyEdit AnyEdit where
    floatingUpdate (MkAnyEdit info1 edit1) aedit2@(MkAnyEdit info2 edit2) =
        case testEquality info1 info2 of
            Just Refl -> MkAnyEdit info2 $ floatingUpdate edit1 edit2
            Nothing -> aedit2

type instance EditReader AnyEdit = AnyReader

instance ApplicableEdit AnyEdit where
    applyEdit (MkAnyEdit ie edit) mr areader@(ReadAnyReader ir rt) = do
        MkAnyTypes oie <- mr ReadAnyTypes
        case (testEquality oie ie, testEquality oie ir) of
            (Just Refl, Just Refl) -> getComposeM $ applyEdit edit (MkComposeM . mr . ReadAnyReader ir) rt
            _ -> mr areader
    applyEdit (MkAnyEdit _ _) mr ReadAnyTypes = mr ReadAnyTypes -- edit cannot change types

instance InvertibleEdit AnyEdit where
    -- invertEdit :: AnyEdit -> Readable AnyReader [AnyEdit];
    invertEdit (MkAnyEdit ie edit) mr = do
        MkAnyTypes oie <- mr ReadAnyTypes
        case testEquality oie ie of
            Just Refl -> do
                minvedits <- getComposeM $ invertEdit edit (MkComposeM . mr . ReadAnyReader oie)
                case minvedits of
                    Nothing -> return []
                    Just invedits -> return $ fmap (MkAnyEdit ie) invedits
            Nothing -> return []

type AnyWholeEdit = SumWholeEdit AnyEdit
