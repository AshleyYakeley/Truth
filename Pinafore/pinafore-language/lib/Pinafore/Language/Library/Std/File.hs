module Pinafore.Language.Library.Std.File
    ( fileLibEntries
    ) where

import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs

--import Changes.World.File
{-
file_import ::
        LangFiniteSetRef '( A, A)
    -> (A -> PinaforeAction ())
    -> PinaforeAction ()
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
                    viewMapEdit (pinaforeFileItemLens entity) $ do
                        let
                            MkResource rr asub = sub
                        liftIO $
                            runWMFunction rr $ do
                                pushEdit $ refEdit [SingleReferenceDeleteCreate]
                                aModelRead asub ReadSingleReferenceStore
                destreference <-
                    case mdestreference of
                        Nothing -> fail "failed to create reference " ++ show entity
                        Just reference -> return reference
                liftIO $ copyReference sourcereference destreference
                continue entity

file_size :: Reference ByteStringEdit -> IO Int64
file_size MkReference {..} = runWMFunction objRun $ refRead ReadByteStringLength
-}
fileLibEntries :: [DocTreeEntry BindDoc]
fileLibEntries =
    [ docTreeEntry
          "File Storage"
          "NYI"
                  {-
                  mkValEntry "file_import" "Import a file into a set." $ file_import
              , mkValEntry "file_size" "The size of a file." file_size
              -}
          []
    ]
