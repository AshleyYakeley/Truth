module Truth.World.Anything where

import Truth.Core
import Truth.Core.Import

data Anything where
    MkAnything :: forall (edit :: *). IOWitness edit -> EditSubject edit -> Anything

data AnyTypes where
    MkAnyTypes :: forall (edit :: *). IOWitness edit -> AnyTypes

data AnyReader t where
    ReadAnyTypes :: AnyReader AnyTypes
    ReadAnyReader
        :: forall edit t. SubjectReader (EditReader edit)
        => IOWitness edit
        -> EditReader edit t
        -> AnyReader (Maybe t)

instance SubjectReader AnyReader where
    type ReaderSubject AnyReader = Anything
    readFromSubject (MkAnything wit _a) ReadAnyTypes = MkAnyTypes wit
    readFromSubject (MkAnything wit a) (ReadAnyReader witr reader) = do
        Refl <- testEquality wit witr
        return (readFromSubject a reader)

data AnyEdit where
    MkAnyEdit
        :: forall edit. (InvertableEdit edit, SubjectReader (EditReader edit))
        => IOWitness edit
        -> edit
        -> AnyEdit

instance Floating AnyEdit AnyEdit where
    floatingUpdate (MkAnyEdit info1 edit1) aedit2@(MkAnyEdit info2 edit2) =
        case testEquality info1 info2 of
            Just Refl -> MkAnyEdit info2 $ floatingUpdate edit1 edit2
            Nothing -> aedit2

instance Edit AnyEdit where
    type EditReader AnyEdit = AnyReader
    applyEdit (MkAnyEdit ie edit) areader@(ReadAnyReader ir reader) = do
        MkAnyTypes oie <- readable ReadAnyTypes
        case (testEquality oie ie, testEquality oie ir) of
            (Just Refl, Just Refl) -> mapReadableF (readable . ReadAnyReader ir) $ applyEdit edit reader
            _ -> readable areader
    applyEdit (MkAnyEdit _ _) ReadAnyTypes = readable ReadAnyTypes -- edit cannot change types

instance InvertableEdit AnyEdit
        -- invertEdit :: AnyEdit -> Readable AnyReader [AnyEdit];
                                                                  where
    invertEdit (MkAnyEdit ie edit) = do
        MkAnyTypes oie <- readable ReadAnyTypes
        case testEquality oie ie of
            Just Refl -> do
                minvedits <- mapReadableF (readable . ReadAnyReader oie) $ invertEdit edit
                case minvedits of
                    Nothing -> return []
                    Just invedits -> return $ fmap (MkAnyEdit ie) invedits
            Nothing -> return []

type AnyWholeEdit = SumWholeEdit AnyEdit
