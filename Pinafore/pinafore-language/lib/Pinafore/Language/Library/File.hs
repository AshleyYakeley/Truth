module Pinafore.Language.Library.File
    ( fileLibEntries
    ) where

import Pinafore.Language.Library.Defs

--import Changes.World.File
{-
file_import ::
        LangFiniteSetModel '( A, A)
    -> (A -> Action ())
    -> Action ()
file_import set continue = do
    chooseFile <- pinaforeActionRequest witChooseFile
    mpath <- liftIO chooseFile
    case mpath of
        Nothing -> return ()
        Just path -> do
            let sourcereference = fileReference path
            newEntity set $ \entity -> do
                mdestreference <-
                    pinaforeLiftView $
                    viewMapEdit (storageFileItemLens entity) $ do
                        let
                            MkResource rr asub = sub
                        liftIO $
                            unWRaised rr $ do
                                pushEdit $ refEdit [SingleReferenceDeleteCreate]
                                aModelRead asub ReadSingleReferenceStore
                destreference <-
                    case mdestreference of
                        Nothing -> fail "failed to create reference " ++ show entity
                        Just reference -> return reference
                liftIO $ copyReference sourcereference destreference
                continue entity

file_size :: Reference ByteStringEdit -> IO Int64
file_size MkReference {..} = unWRaised objRun $ refRead ReadByteStringLength
-}
fileLibEntries :: [BindDocStuff context]
fileLibEntries =
    [ headingBDS
          "File Storage"
          "NYI"
                  {-
                  valBDS "file_import" "Import a file into a set." $ file_import
              , valBDS "file_size" "The size of a file." file_size
              -}
          []
    ]
