module Pinafore.Language.Predefined.File
    ( file_predefinitions
    ) where

import Pinafore.Base
import Pinafore.Language.Doc
import Pinafore.Language.Predefined.Defs
import Pinafore.Storage.File

--import Truth.World.File
{-
file_import ::
       forall baseedit. HasPinaforeFileEdit baseedit
    => PinaforeSet baseedit '( A, A)
    -> (A -> PinaforeAction baseedit ())
    -> PinaforeAction baseedit ()
file_import set continue = do
    chooseFile <- pinaforeActionRequest witChooseFile
    mpath <- liftIO chooseFile
    case mpath of
        Nothing -> return ()
        Just path -> do
            let sourceobject = fileObject path
            newentity set $ \entity -> do
                mdestobject <-
                    pinaforeLiftView $
                    viewMapEdit (pinaforeFileItemLens entity) $ do
                        MkObject {..} <- viewObject
                        liftIO $
                            runTransform objRun $ do
                                pushEdit $ objEdit [SingleObjectDeleteCreate]
                                objRead ReadSingleObjectStore
                destobject <-
                    case mdestobject of
                        Nothing -> pinaforeLiftResult $ FailureResult $ fromString $ "failed to create object " ++ show entity
                        Just object -> return object
                liftIO $ copyObject sourceobject destobject
                continue entity

file_size :: Object ByteStringEdit -> IO Int64
file_size MkObject {..} = runTransform objRun $ objRead ReadByteStringLength
-}
file_predefinitions ::
       forall baseedit. (HasPinaforeEntityEdit baseedit, HasPinaforeFileEdit baseedit)
    => DocTree (BindDoc baseedit)
file_predefinitions =
    MkDocTree
        "Files"
        "NYI"
                  {-
                  mkValEntry "file_import" "Import a file into a set." $ file_import @baseedit
              , mkValEntry "file_size" "The size of a file." file_size
              -}
        []
