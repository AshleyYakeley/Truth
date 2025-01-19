module Changes.Core.Types.Either
    ( EitherReader(..)
    , EitherEdit(..)
    ) where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Read

data EitherReader (ra :: Type -> Type) (rb :: Type -> Type) (t :: Type) where
    EitherReadIsRight :: EitherReader ra rb Bool
    EitherReadLeft :: ra t -> EitherReader ra rb (Maybe t)
    EitherReadRight :: rb t -> EitherReader ra rb (Maybe t)

mapEitherReadLeft :: ReadFunctionF Maybe (EitherReader ra rb) ra
mapEitherReadLeft mr = MkComposeInner . mr . EitherReadLeft

mapEitherReadRight :: ReadFunctionF Maybe (EitherReader ra rb) rb
mapEitherReadRight mr = MkComposeInner . mr . EitherReadRight

instance (SubjectReader ra, SubjectReader rb) => SubjectReader (EitherReader ra rb) where
    type ReaderSubject (EitherReader ra rb) = Either (ReaderSubject ra) (ReaderSubject rb)
    subjectToRead (Left _) EitherReadIsRight = False
    subjectToRead (Right _) EitherReadIsRight = True
    subjectToRead (Left a) (EitherReadLeft rd) = Just $ subjectToRead a rd
    subjectToRead (Right _) (EitherReadLeft _) = Nothing
    subjectToRead (Left _) (EitherReadRight _) = Nothing
    subjectToRead (Right a) (EitherReadRight rd) = Just $ subjectToRead a rd

instance (FullSubjectReader ra, FullSubjectReader rb) => FullSubjectReader (EitherReader ra rb) where
    readableToSubject mr = do
        mleft <- unComposeInner $ readableToSubject $ mapEitherReadLeft mr
        mright <- unComposeInner $ readableToSubject $ mapEitherReadRight mr
        case (mleft, mright) of
            (Just a, Nothing) -> return $ Left a
            (Nothing, Just a) -> return $ Right a
            _ -> error $ "pureFromReader: inconsistent EitherReader"

-- | note this edit cannot switch the subject between the left and right branches
data EitherEdit ea eb
    = EitherEditLeft ea
    | EitherEditRight eb

partitionEitherEdits :: forall ea eb. [EitherEdit ea eb] -> ([ea], [eb])
partitionEitherEdits pes = let
    toEither :: EitherEdit ea eb -> Either ea eb
    toEither (EitherEditLeft ea) = Left ea
    toEither (EitherEditRight eb) = Right eb
    in partitionEithers $ fmap toEither pes

instance (FloatingOn ea ea, FloatingOn eb eb) => FloatingOn (EitherEdit ea eb) (EitherEdit ea eb) where
    floatingUpdate (EitherEditLeft e1) (EitherEditLeft e2) = EitherEditLeft $ floatingUpdate e1 e2
    floatingUpdate (EitherEditRight e1) (EitherEditRight e2) = EitherEditRight $ floatingUpdate e1 e2
    floatingUpdate _ t = t

type instance EditReader (EitherEdit ea eb) = EitherReader (EditReader ea) (EditReader eb)

instance (ApplicableEdit ea, ApplicableEdit eb) => ApplicableEdit (EitherEdit ea eb) where
    applyEdit (EitherEditLeft edit) mr (EitherReadLeft rd) = unComposeInner $ applyEdit edit (mapEitherReadLeft mr) rd
    applyEdit (EitherEditRight edit) mr (EitherReadRight rd) =
        unComposeInner $ applyEdit edit (mapEitherReadRight mr) rd
    applyEdit _ mr rd = mr rd

instance (InvertibleEdit ea, InvertibleEdit eb) => InvertibleEdit (EitherEdit ea eb) where
    invertEdits edits mr = do
        let (eas, ebs) = partitionEitherEdits edits
        meditas <- unComposeInner $ invertEdits eas $ mapEitherReadLeft mr
        meditbs <- unComposeInner $ invertEdits ebs $ mapEitherReadRight mr
        return $ (fmap EitherEditLeft $ fromMaybe [] meditas) ++ (fmap EitherEditRight $ fromMaybe [] meditbs)
