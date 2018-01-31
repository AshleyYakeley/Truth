{-# OPTIONS -fno-warn-redundant-constraints #-}

module Truth.Core.Types.Either where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read

data EitherReader (ra :: * -> *) (rb :: * -> *) (t :: *) where
    EitherReadIsRight :: EitherReader ra rb Bool
    EitherReadLeft :: ra t -> EitherReader ra rb (Maybe t)
    EitherReadRight :: rb t -> EitherReader ra rb (Maybe t)

mapEitherReadLeft :: ReadFunctionF Maybe (EitherReader ra rb) ra
mapEitherReadLeft mr = Compose . mr . EitherReadLeft

mapEitherReadRight :: ReadFunctionF Maybe (EitherReader ra rb) rb
mapEitherReadRight mr = Compose . mr . EitherReadRight

instance (SubjectReader ra, SubjectReader rb) => SubjectReader (EitherReader ra rb) where
    type ReaderSubject (EitherReader ra rb) = Either (ReaderSubject ra) (ReaderSubject rb)
    subjectToRead (Left _) EitherReadIsRight = False
    subjectToRead (Right _) EitherReadIsRight = True
    subjectToRead (Left a) (EitherReadLeft reader) = Just $ subjectToRead a reader
    subjectToRead (Right _) (EitherReadLeft _) = Nothing
    subjectToRead (Left _) (EitherReadRight _) = Nothing
    subjectToRead (Right a) (EitherReadRight reader) = Just $ subjectToRead a reader

instance (FullSubjectReader ra, FullSubjectReader rb) => FullSubjectReader (EitherReader ra rb) where
    mutableReadToSubject mr = do
        mleft <- getCompose $ mutableReadToSubject $ mapEitherReadLeft mr
        mright <- getCompose $ mutableReadToSubject $ mapEitherReadRight mr
        case (mleft, mright) of
            (Just a, Nothing) -> return $ Left a
            (Nothing, Just a) -> return $ Right a
            _ -> error $ "pureFromReader: inconsistent EitherReader"
    -- note this edit cannot switch the subject between the left and right branches

data EitherEdit ea eb
    = EitherEditLeft ea
    | EitherEditRight eb

instance (Floating ea ea, Floating eb eb) => Floating (EitherEdit ea eb) (EitherEdit ea eb) where
    floatingUpdate (EitherEditLeft e1) (EitherEditLeft e2) = EitherEditLeft $ floatingUpdate e1 e2
    floatingUpdate (EitherEditRight e1) (EitherEditRight e2) = EitherEditRight $ floatingUpdate e1 e2
    floatingUpdate _ t = t

instance (Edit ea, Edit eb) => Edit (EitherEdit ea eb) where
    type EditReader (EitherEdit ea eb) = EitherReader (EditReader ea) (EditReader eb)
    applyEdit (EitherEditLeft edit) mr (EitherReadLeft reader) =
        getCompose $ applyEdit edit (mapEitherReadLeft mr) reader
    applyEdit (EitherEditRight edit) mr (EitherReadRight reader) =
        getCompose $ applyEdit edit (mapEitherReadRight mr) reader
    applyEdit _ mr reader = mr reader

instance (InvertibleEdit ea, InvertibleEdit eb) => InvertibleEdit (EitherEdit ea eb) where
    invertEdit (EitherEditLeft edit) mr = do
        medits <- getCompose $ invertEdit edit $ mapEitherReadLeft mr
        case medits of
            Just edits -> return $ fmap EitherEditLeft edits
            Nothing -> return []
    invertEdit (EitherEditRight edit) mr = do
        medits <- getCompose $ invertEdit edit $ mapEitherReadRight mr
        case medits of
            Just edits -> return $ fmap EitherEditRight edits
            Nothing -> return []