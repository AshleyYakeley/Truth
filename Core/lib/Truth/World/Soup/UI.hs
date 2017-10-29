module Truth.World.Soup.UI(PossibleNoteEdit,soupWindow) where
{
    import Truth.Core.Import;
    import System.FilePath hiding ((<.>));
    import Truth.Core;
    import Truth.World.FileSystem;
    import Truth.World.Soup.Note;
    import Truth.World.Soup.Edit;


    fromResult :: Result String String -> String;
    fromResult (SuccessResult s) = s;
    fromResult (FailureResult s) = "<" ++ s ++ ">";

    pastResult :: Result String Bool -> String;
    pastResult (SuccessResult False) = "current";
    pastResult (SuccessResult True) = "past";
    pastResult (FailureResult s) = "<" ++ s ++ ">";

    type PossibleNoteEdit = OneWholeEdit (Result String) NoteEdit;
    soupEditSpec :: UISpec (SoupEdit PossibleNoteEdit);
    soupEditSpec = let
    {
        nameColumn :: KeyColumn (SoupEdit PossibleNoteEdit) UUID;
        nameColumn = MkKeyColumn "Name" $ \key -> do
        {
            lens <- getKeyElementGeneralLens key;
            return $ readOnlyGeneralLens (funcGeneralFunction fromResult) <.> oneWholeLiftGeneralLens (tupleGeneralLens NoteTitle) <.> mustExistOneGeneralLens "name" <.> oneWholeLiftGeneralLens (tupleGeneralLens EditSecond) <.> lens;
        };

        pastColumn :: KeyColumn (SoupEdit PossibleNoteEdit) UUID;
        pastColumn = MkKeyColumn "Past" $ \key -> do
        {
            lens <- getKeyElementGeneralLens key;
            return $ readOnlyGeneralLens (funcGeneralFunction pastResult) <.> oneWholeLiftGeneralLens (tupleGeneralLens NotePast) <.> mustExistOneGeneralLens "past" <.> oneWholeLiftGeneralLens (tupleGeneralLens EditSecond) <.> lens;
        };

        getaspect :: Aspect (MaybeEdit (UUIDElementEdit PossibleNoteEdit));
        getaspect = return $ Just $ ("item", uiLens (oneWholeLiftGeneralLens $ tupleGeneralLens EditSecond) $ uiOneWhole $ uiOneWhole noteEditSpec);
    } in uiSimpleTable [nameColumn,pastColumn] getaspect;

    soupObject :: FilePath -> Object (SoupEdit PossibleNoteEdit);
    soupObject dirpath = let
    {
        rawSoupObject :: Object (SoupEdit (MutableIOEdit ByteStringEdit));
        rawSoupObject = directorySoup fileSystemMutableEdit dirpath;

        soupItemInjection :: Injection' (Result String) ByteString (EditSubject PossibleNoteEdit);
        soupItemInjection = codecInjection noteCodec;

        paste :: forall m. MonadIO m => EditSubject PossibleNoteEdit -> m (Maybe ByteString);
        paste s = return $ getMaybeOne $ injBackwards soupItemInjection s;

        soupItemLens :: PureEditLens ByteStringEdit PossibleNoteEdit;
        soupItemLens = convertEditLens <.> (wholeEditLens $ injectionLens soupItemInjection) <.> convertEditLens;

        lens :: GeneralLens (SoupEdit (MutableIOEdit ByteStringEdit)) (SoupEdit PossibleNoteEdit);
        lens = MkCloseState $ liftSoupLens paste $ soupItemLens <.> mutableIOEditLens;
    } in mapObject lens rawSoupObject;

    soupWindow :: FilePath -> IO (UIWindow ());
    soupWindow dirpath = do
    {
        let
        {
            uiwTitle = takeFileName $ dropTrailingPathSeparator dirpath;
            uiwSpec = soupEditSpec;
        };
        uiwSubscriber <- makeObjectSubscriber $ soupObject dirpath;
        return $ MkUIWindow{..};
    };
}
