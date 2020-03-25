module Pinafore.Language.Predefined.File
    ( file_predefinitions
    ) where

import Pinafore.Base
import Pinafore.Language.DocTree
import Pinafore.Language.Predefined.Defs
import Pinafore.Storage

--import Truth.World.File
{-
file_import ::
       forall baseupdate. HasPinaforeFileUpdate baseupdate
    => PinaforeFiniteSetRef '( A, A)
    -> (A -> PinaforeAction ())
    -> PinaforeAction ()
file_import set continue = do
    chooseFile <- pinaforeActionRequest witChooseFile
    mpath <- liftIO chooseFile
    case mpath of
        Nothing -> return ()
        Just path -> do
            let sourceobject = fileObject path
            newEntity set $ \entity -> do
                mdestobject <-
                    pinaforeLiftView $
                    viewMapEdit (pinaforeFileItemLens entity) $ do
                        let
                            MkResource rr asub = sub
                        liftIO $
                            runWMFunction rr $ do
                                pushEdit $ objEdit [SingleObjectDeleteCreate]
                                aModelRead asub ReadSingleObjectStore
                destobject <-
                    case mdestobject of
                        Nothing -> fail "failed to create object " ++ show entity
                        Just object -> return object
                liftIO $ copyObject sourceobject destobject
                continue entity

file_size :: Object ByteStringEdit -> IO Int64
file_size MkObject {..} = runWMFunction objRun $ objRead ReadByteStringLength
-}
file_predefinitions ::
       forall baseupdate. (HasPinaforeEntityUpdate baseupdate, HasPinaforeFileUpdate baseupdate)
    => [DocTreeEntry (BindDoc baseupdate)]
file_predefinitions =
    [ docTreeEntry
          "Files"
          "NYI"
                  {-
                  mkValEntry "file_import" "Import a file into a set." $ file_import @baseupdate
              , mkValEntry "file_size" "The size of a file." file_size
              -}
          []
    ]
