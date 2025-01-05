module Changes.Core.Types.Key.HasKey
    ( HasKeyReader(..)
    , HasKeyUpdate(..)
    ) where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Read
import Changes.Core.Types.None
import Changes.Core.Types.Tuple.Context
import Changes.Core.Types.Tuple.Pair
import Changes.Core.Types.Tuple.Tuple
import Changes.Core.Types.Whole

class KeyContainer cont => HasKeyReader cont reader where
    readKey ::
           forall m. MonadIO m
        => Readable m reader
        -> m (ContainerKey cont)

instance (KeyContainer cont, HasKeyReader cont (UpdateReader updateN)) =>
             HasKeyReader cont (ContextUpdateReader updateX updateN) where
    readKey mr = readKey @cont $ tupleReadFunction SelectContent mr

instance Eq t => HasKeyReader (ListSet t) (WholeReader t) where
    readKey mr = mr ReadWhole

instance ( Eq key
         , UpdateSubject keyupdate ~ key
         , UpdateSubject valupdate ~ val
         , FullSubjectReader (UpdateReader keyupdate)
         ) => HasKeyReader [(key, val)] (PairUpdateReader keyupdate valupdate) where
    readKey mr = readableToSubject $ firstReadFunction mr

class HasKeyReader cont (UpdateReader update) => HasKeyUpdate cont update where
    updatesKey :: update -> Maybe (ContainerKey cont -> IO (ContainerKey cont))

instance HasKeyReader cont r => HasKeyUpdate cont (ConstUpdate r) where
    updatesKey = never

instance Eq t => HasKeyUpdate (ListSet t) (WholeUpdate t) where
    updatesKey (MkWholeReaderUpdate a) = Just $ \_ -> return a

instance ( Eq key
         , UpdateSubject keyupdate ~ key
         , UpdateSubject valupdate ~ val
         , FullSubjectReader (UpdateReader keyupdate)
         , IsEditUpdate keyupdate
         , ApplicableEdit (UpdateEdit keyupdate)
         ) => HasKeyUpdate [(key, val)] (PairUpdate keyupdate valupdate) where
    updatesKey (MkTupleUpdate SelectFirst update) =
        Just $ \oldKey -> readableToSubject $ applyEdit (updateEdit update) $ subjectToReadable oldKey
    updatesKey _ = Nothing
